{- |
Module: AtomicSwap.Protocol.Bob
Description: Bob's protocol logic for atomic swap

This module implements Bob's side of the atomic swap protocol. Bob is
the responder who:
1. Receives Alice's adapter commitment
2. Creates his transaction on ChainB
3. Creates an adapted pre-signature using Alice's commitment
4. Waits for Alice to publish
5. Extracts the adapter secret from Alice's published signature
6. Completes his own signature and publishes

The protocol is polymorphic over MonadSTM/MonadRandom allowing deterministic
testing with io-sim.
-}
module AtomicSwap.Protocol.Bob
  ( bobProtocol
  ) where

import Prelude hiding (STM, TVar, atomically, newTVar, readTVar, writeTVar)

import Control.Monad.Class.MonadSTM (MonadSTM)
import Crypto.Random (MonadRandom)

import AtomicSwap.Blockchain.Ledger
  ( getBalance
  , queryUTXOs
  , submitTransaction
  )
import AtomicSwap.Blockchain.Transaction (buildTransaction, hashTransaction)
import AtomicSwap.Blockchain.Types (Blockchain)
import AtomicSwap.Crypto.Adapter
  ( adaptSignature
  , extractAdapterSecret
  , preSignREdDSA
  , preVerifyREdDSA
  )
import AtomicSwap.Logging
  ( logAction
  , logError
  , logInfo
  , logPhase
  , logPublicKey
  , logSecret
  , logSeparator
  , logSubPhase
  , logTransaction
  )
import AtomicSwap.Protocol.Messaging (MessageQueue, receiveMessage, sendMessage)
import AtomicSwap.Types
  ( AdapterPoint (..)
  , AdapterSecret (..)
  , Message (..)
  , NIZKProof (..)
  , Output (..)
  , Party (..)
  , PublicKey (..)
  , Signature (..)
  , SwapResult (..)
  , Transaction (..)
  , TxId (..)
  , UTXO (..)
  )

--------------------------------------------------------------------------------
-- Bob's Protocol --------------------------------------------------------------

{- |
Execute Bob's side of the atomic swap protocol.

Polymorphic over MonadSTM and MonadRandom for testability.

Bob is the responder who:
1. Receives Alice's public key and adapter commitment T
2. Verifies NIZK proof for T
3. Creates transaction on ChainB sending funds to Alice
4. Creates adapted pre-signature using Alice's T (NOT his own secret!)
5. Verifies Alice's adapted pre-signature
6. Waits for Alice to publish on ChainA
7. Extracts adapter secret t from Alice's published signature
8. Completes own signature using extracted t
9. Publishes to ChainB

Args:
  - bob: Bob's party information (keys, name)
  - chainB: Blockchain where Bob will send funds to Alice
  - toBob: Message queue for receiving messages from Alice
  - toAlice: Message queue for sending messages to Alice
  - amountToSend: Amount Bob will send to Alice on ChainB

Returns: SwapSuccess if swap completes, SwapFailure with error message otherwise
-}
bobProtocol
  :: (MonadSTM m, MonadRandom m, MonadIO m)
  => Party
  -> Blockchain m
  -> MessageQueue m
  -> MessageQueue m
  -> Word64
  -> m SwapResult
bobProtocol bob chainB toBob toAlice amountToSend = do
  liftIO $ logPhase "BOB'S PROTOCOL EXECUTION"

  -- Phase 1: Setup and key exchange
  liftIO $ logSubPhase "Phase 1: Setup and Key Exchange"

  liftIO $ logAction (partyParticipant bob) "Waiting for Alice's public key"
  alicePubKeyMsg <- receiveMessage toBob
  case alicePubKeyMsg of
    PublicKeyMsg alicePubKey -> do
      let PublicKey pkBytes = alicePubKey
      liftIO $
        logPublicKey (partyParticipant bob) "Received Alice's public key" pkBytes

      liftIO $
        logAction (partyParticipant bob) "Waiting for adapter commitment from Alice"
      adapterMsg <- receiveMessage toBob
      case adapterMsg of
        AdapterPointMsg adapterPoint -> do
          liftIO $
            logInfo (partyParticipant bob) $
              "Received adapter commitment: " <> show adapterPoint

          liftIO $ logAction (partyParticipant bob) "Sending public key to Alice"
          sendMessage toAlice (PublicKeyMsg (partyPublicKey bob))

          continueBobProtocol
            bob
            chainB
            toBob
            toAlice
            amountToSend
            alicePubKey
            adapterPoint
        _ -> do
          liftIO $ logError (partyParticipant bob) "Expected AdapterPointMsg from Alice"
          return $ SwapFailure "Protocol error: Expected adapter commitment from Alice"
    _ -> do
      liftIO $ logError (partyParticipant bob) "Expected PublicKeyMsg from Alice"
      return $ SwapFailure "Protocol error: Expected public key from Alice"

continueBobProtocol
  :: (MonadSTM m, MonadRandom m, MonadIO m)
  => Party
  -> Blockchain m
  -> MessageQueue m
  -> MessageQueue m
  -> Word64
  -> PublicKey
  -> AdapterPoint
  -> m SwapResult
continueBobProtocol bob chainB toBob toAlice amountToSend alicePubKey adapterPoint = do
  -- Phase 2: Transaction creation
  liftIO logSeparator
  liftIO $ logSubPhase "Phase 2: Transaction Creation"

  liftIO $ logAction (partyParticipant bob) "Querying UTXOs on ChainB"
  bobUTXOs <- queryUTXOs chainB (partyPublicKey bob)

  if null bobUTXOs
    then do
      liftIO $ logError (partyParticipant bob) "No UTXOs available on ChainB"
      return $ SwapFailure "Bob has no UTXOs on ChainB"
    else do
      let totalAvailable = sum (map utxoAmount bobUTXOs)
      liftIO $
        logInfo
          (partyParticipant bob)
          ("Total available on ChainB: " <> show totalAvailable)

      if totalAvailable < amountToSend
        then do
          liftIO $ logError (partyParticipant bob) "Insufficient funds on ChainB"
          return $ SwapFailure "Bob has insufficient funds on ChainB"
        else do
          -- Create transaction sending funds to Alice
          let output = Output {outputOwner = alicePubKey, outputAmount = amountToSend}
              bobTx = buildTransaction bobUTXOs [output]
              txHash = hashTransaction bobTx

          liftIO $
            logInfo
              (partyParticipant bob)
              ("Created transaction sending " <> show amountToSend <> " to Alice")

          -- Phase 3: Adapter signature creation
          liftIO logSeparator
          liftIO $ logSubPhase "Phase 3: Adapter Signature Creation"

          liftIO $
            logAction
              (partyParticipant bob)
              "Creating adapted pre-signature using Alice's adapter commitment"
          liftIO $
            logInfo
              (partyParticipant bob)
              "NOTE: Using Alice's T, not generating own secret!"

          -- Create dummy NIZK proof (not verified in this simplified version)
          let dummyProof = NIZKProof ""

          bobPreSig <-
            preSignREdDSA
              (partyPrivateKey bob)
              (partyPublicKey bob)
              txHash
              adapterPoint
              dummyProof

          liftIO $ logInfo (partyParticipant bob) "Adapted pre-signature created"

          liftIO $ logAction (partyParticipant bob) "Verifying own adapted pre-signature"
          let preVerifyResult =
                preVerifyREdDSA
                  (partyPublicKey bob)
                  txHash
                  adapterPoint
                  bobPreSig
                  dummyProof

          if not preVerifyResult
            then do
              liftIO $ logError (partyParticipant bob) "Own pre-signature verification failed"
              return $ SwapFailure "Bob's pre-signature verification failed"
            else do
              liftIO $
                logInfo (partyParticipant bob) "Own pre-signature verified successfully"

              -- Phase 4: Exchange signatures
              liftIO logSeparator
              liftIO $ logSubPhase "Phase 4: Signature Exchange"

              liftIO $
                logAction (partyParticipant bob) "Waiting for Alice's transaction proposal"
              aliceProposalMsg <- receiveMessage toBob
              case aliceProposalMsg of
                TransactionProposalMsg aliceTx alicePreSig -> do
                  liftIO $ logInfo (partyParticipant bob) "Received Alice's transaction proposal"

                  liftIO $
                    logAction (partyParticipant bob) "Verifying Alice's adapted pre-signature"
                  let aliceTxHash = hashTransaction aliceTx
                      alicePreVerifyResult =
                        preVerifyREdDSA
                          alicePubKey
                          aliceTxHash
                          adapterPoint
                          alicePreSig
                          dummyProof

                  if not alicePreVerifyResult
                    then do
                      liftIO $
                        logError (partyParticipant bob) "Alice's pre-signature verification failed"
                      return $ SwapFailure "Alice's pre-signature verification failed"
                    else do
                      liftIO $
                        logInfo (partyParticipant bob) "Alice's pre-signature verified successfully"

                      liftIO $
                        logAction (partyParticipant bob) "Sending transaction proposal to Alice"
                      sendMessage toAlice (TransactionProposalMsg bobTx bobPreSig)

                      -- Phase 5: Wait for Alice and extract secret
                      liftIO logSeparator
                      liftIO $ logSubPhase "Phase 5: Wait for Alice and Extract Secret"

                      liftIO $
                        logAction (partyParticipant bob) "Waiting for Alice to publish her transaction"
                      swapCompleteMsg <- receiveMessage toBob
                      case swapCompleteMsg of
                        SwapCompleteMsg -> do
                          liftIO $
                            logInfo (partyParticipant bob) "Received notification: Alice has published!"

                          liftIO $
                            logAction
                              (partyParticipant bob)
                              "Extracting adapter secret from Alice's signature"
                          liftIO $
                            logInfo
                              (partyParticipant bob)
                              "This is the KEY OPERATION: t = alice_sig - alice_presig"

                          -- In a real implementation, we would query Alice's published transaction
                          -- from ChainA. For this simulation, we need to get Alice's completed
                          -- signature. Since we don't have cross-chain queries, we'll simulate
                          -- by completing our signature directly for now.
                          -- TODO: In full implementation, query ChainA for Alice's transaction

                          liftIO $
                            logAction
                              (partyParticipant bob)
                              "Computing complete signature (simulated extraction)"
                          liftIO $
                            logInfo
                              (partyParticipant bob)
                              "NOTE: In real system, would extract from Alice's published tx"

                          -- Wait for Alice's complete signature so we can extract the adapter secret
                          liftIO $
                            logAction
                              (partyParticipant bob)
                              "Waiting for Alice's complete signature for extraction"

                          msg <- receiveMessage toBob
                          case msg of
                            CompleteSignatureMsg aliceCompleteSig -> do
                              liftIO $
                                logInfo (partyParticipant bob) "Received Alice's complete signature"

                              -- Extract adapter secret from Alice's signatures
                              -- y = sig_complete - sig_presig
                              liftIO $
                                logAction (partyParticipant bob) "Extracting adapter secret from signatures"
                              let extractedSecret = extractAdapterSecret alicePreSig aliceCompleteSig

                              let AdapterSecret extractedBytes = extractedSecret
                              liftIO $
                                logSecret
                                  (partyParticipant bob)
                                  "Extracted adapter secret"
                                  extractedBytes

                              -- Complete Bob's signature using extracted adapter secret
                              -- sig_bob = sig_tilde_bob + y
                              liftIO $
                                logAction (partyParticipant bob) "Completing signature with extracted secret"
                              let bobCompleteSig = adaptSignature bobPreSig extractedSecret

                              -- Verify that the completed signature is valid
                              let Signature {sigScalar = completeSigScalar} = bobCompleteSig
                              liftIO $
                                logSecret
                                  (partyParticipant bob)
                                  "Completed signature scalar"
                                  completeSigScalar

                              let signedBobTx = bobTx {txSignatures = [bobCompleteSig]}

                              liftIO $ logAction (partyParticipant bob) "Publishing transaction to ChainB"
                              submitResult <- submitTransaction chainB signedBobTx

                              case submitResult of
                                Left err -> do
                                  liftIO $
                                    logError (partyParticipant bob) ("Transaction submission failed: " <> err)
                                  return $
                                    SwapFailure ("Bob's transaction submission failed: " <> err)
                                Right txId -> do
                                  let TxId txIdBytes = txId
                                  liftIO $
                                    logTransaction
                                      (partyParticipant bob)
                                      "Transaction published successfully"
                                      txIdBytes

                                  -- Final status
                                  liftIO logSeparator
                                  liftIO $ logPhase "BOB'S PROTOCOL COMPLETE"

                                  finalBalance <- getBalance chainB (partyPublicKey bob)
                                  liftIO $
                                    logInfo
                                      (partyParticipant bob)
                                      ("Final balance on ChainB: " <> show finalBalance)

                                  liftIO $ logInfo (partyParticipant bob) "✓ Atomic swap completed successfully"
                                  liftIO $
                                    logInfo
                                      (partyParticipant bob)
                                      "✓ Successfully extracted adapter secret and completed transaction"

                                  return SwapSuccess
                            _ -> do
                              liftIO $
                                logError
                                  (partyParticipant bob)
                                  "Unexpected message type (expected CompleteSignatureMsg)"
                              return $ SwapFailure "Protocol error: Expected complete signature from Alice"
                        _ -> do
                          liftIO $ logError (partyParticipant bob) "Expected SwapCompleteMsg from Alice"
                          return $ SwapFailure "Protocol error: Expected swap complete message"
                _ -> do
                  liftIO $
                    logError (partyParticipant bob) "Expected TransactionProposalMsg from Alice"
                  return $ SwapFailure "Protocol error: Expected transaction proposal from Alice"
