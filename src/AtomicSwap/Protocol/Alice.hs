{- |
Module: AtomicSwap.Protocol.Alice
Description: Alice's protocol logic for atomic swap

This module implements Alice's side of the atomic swap protocol. Alice is
the initiator who:
1. Generates the adapter secret and commitment
2. Creates her transaction on ChainA
3. Creates an adapted pre-signature
4. Publishes first (revealing the adapter secret)

The protocol is polymorphic over MonadSTM/MonadRandom allowing deterministic
testing with io-sim.
-}
module AtomicSwap.Protocol.Alice
  ( aliceProtocol
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
  , generateAdapterCommitment
  , generateAdapterSecret
  , preSignREdDSA
  , preVerifyREdDSA
  )
import AtomicSwap.Crypto.NIZK (proveDiscreteLog)
import AtomicSwap.Crypto.Signatures (verifyREdDSA)
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
  , NIZKProof
  , Output (..)
  , Party (..)
  , PublicKey (..)
  , SwapResult (..)
  , Transaction (..)
  , TxId (..)
  , UTXO (..)
  )

--------------------------------------------------------------------------------
-- Alice's Protocol ------------------------------------------------------------

{- |
Execute Alice's side of the atomic swap protocol.

Polymorphic over MonadSTM and MonadRandom for testability.

Alice is the initiator and swap leader who:
1. Generates adapter secret t and commitment T = t·B
2. Exchanges public keys with Bob
3. Creates transaction on ChainA sending funds to Bob
4. Creates adapted pre-signature for her transaction
5. Verifies Bob's adapted pre-signature
6. Completes her signature (revealing t)
7. Publishes to ChainA first

Args:
  - alice: Alice's party information (keys, name)
  - chainA: Blockchain where Alice will send funds to Bob
  - toAlice: Message queue for receiving messages from Bob
  - toBob: Message queue for sending messages to Bob
  - amountToSend: Amount Alice will send to Bob on ChainA

Returns: SwapSuccess if swap completes, SwapFailure with error message otherwise
-}
aliceProtocol
  :: (MonadSTM m, MonadRandom m, MonadIO m)
  => Party
  -> Blockchain m
  -> MessageQueue m
  -> MessageQueue m
  -> Word64
  -> m SwapResult
aliceProtocol alice chainA toAlice toBob amountToSend = do
  liftIO $ logPhase "ALICE'S PROTOCOL EXECUTION"

  -- Phase 1: Setup and key exchange
  liftIO $ logSubPhase "Phase 1: Setup and Key Exchange"

  liftIO $ logAction (partyName alice) "Generating adapter secret and commitment"
  adapterSecret <- generateAdapterSecret
  let adapterPoint = generateAdapterCommitment adapterSecret
      AdapterSecret secretBytes = adapterSecret

  liftIO $ logSecret (partyName alice) "Adapter secret (t)" secretBytes
  liftIO $
    logInfo (partyName alice) $
      "Adapter commitment (T): " <> show adapterPoint

  liftIO $
    logAction (partyName alice) "Generating NIZK proof for adapter commitment"
  nizkProof <- proveDiscreteLog adapterSecret adapterPoint

  liftIO $ logAction (partyName alice) "Sending public key to Bob"
  sendMessage toBob (PublicKeyMsg (partyPublicKey alice))

  liftIO $
    logAction (partyName alice) "Sending adapter commitment and proof to Bob"
  sendMessage toBob (AdapterPointMsg adapterPoint)

  liftIO $ logAction (partyName alice) "Waiting for Bob's public key"
  bobPubKeyMsg <- receiveMessage toAlice
  case bobPubKeyMsg of
    PublicKeyMsg bobPubKey -> do
      let PublicKey pkBytes = bobPubKey
      liftIO $ logPublicKey (partyName alice) "Received Bob's public key" pkBytes
      continueProtocol
        alice
        chainA
        toAlice
        toBob
        amountToSend
        bobPubKey
        adapterSecret
        adapterPoint
        nizkProof
    _ -> do
      liftIO $ logError (partyName alice) "Expected PublicKeyMsg from Bob"
      return $ SwapFailure "Protocol error: Expected public key from Bob"

continueProtocol
  :: (MonadSTM m, MonadRandom m, MonadIO m)
  => Party
  -> Blockchain m
  -> MessageQueue m
  -> MessageQueue m
  -> Word64
  -> PublicKey
  -> AdapterSecret
  -> AdapterPoint
  -> NIZKProof
  -> m SwapResult
continueProtocol alice chainA toAlice toBob amountToSend bobPubKey adapterSecret adapterPoint nizkProof = do
  -- Phase 2: Transaction creation
  liftIO logSeparator
  liftIO $ logSubPhase "Phase 2: Transaction Creation"

  liftIO $ logAction (partyName alice) "Querying UTXOs on ChainA"
  aliceUTXOs <- queryUTXOs chainA (partyPublicKey alice)

  if null aliceUTXOs
    then do
      liftIO $ logError (partyName alice) "No UTXOs available on ChainA"
      return $ SwapFailure "Alice has no UTXOs on ChainA"
    else do
      let totalAvailable = sum (map utxoAmount aliceUTXOs)
      liftIO $
        logInfo
          (partyName alice)
          ("Total available on ChainA: " <> show totalAvailable)

      if totalAvailable < amountToSend
        then do
          liftIO $ logError (partyName alice) "Insufficient funds on ChainA"
          return $ SwapFailure "Alice has insufficient funds on ChainA"
        else do
          -- Create transaction sending funds to Bob
          let output = Output {outputOwner = bobPubKey, outputAmount = amountToSend}
              aliceTx = buildTransaction aliceUTXOs [output]
              txHash = hashTransaction aliceTx

          liftIO $
            logInfo
              (partyName alice)
              ("Created transaction sending " <> show amountToSend <> " to Bob")

          -- Phase 3: Adapter signature creation
          liftIO logSeparator
          liftIO $ logSubPhase "Phase 3: Adapter Signature Creation"

          liftIO $
            logAction (partyName alice) "Creating adapted pre-signature for transaction"
          alicePreSig <-
            preSignREdDSA
              (partyPrivateKey alice)
              (partyPublicKey alice)
              txHash
              adapterPoint
              nizkProof

          liftIO $ logInfo (partyName alice) "Adapted pre-signature created"

          liftIO $ logAction (partyName alice) "Verifying own adapted pre-signature"
          let preVerifyResult =
                preVerifyREdDSA
                  (partyPublicKey alice)
                  txHash
                  adapterPoint
                  alicePreSig
                  nizkProof

          if not preVerifyResult
            then do
              liftIO $ logError (partyName alice) "Own pre-signature verification failed"
              return $ SwapFailure "Alice's pre-signature verification failed"
            else do
              liftIO $ logInfo (partyName alice) "Own pre-signature verified successfully"

              -- Phase 4: Exchange signatures
              liftIO logSeparator
              liftIO $ logSubPhase "Phase 4: Signature Exchange"

              liftIO $
                logAction
                  (partyName alice)
                  "Sending transaction proposal and pre-signature to Bob"
              sendMessage toBob (TransactionProposalMsg aliceTx alicePreSig)

              liftIO $ logAction (partyName alice) "Waiting for Bob's transaction proposal"
              bobProposalMsg <- receiveMessage toAlice
              case bobProposalMsg of
                TransactionProposalMsg bobTx bobPreSig -> do
                  liftIO $ logInfo (partyName alice) "Received Bob's transaction proposal"

                  liftIO $ logAction (partyName alice) "Verifying Bob's adapted pre-signature"
                  let bobTxHash = hashTransaction bobTx
                      bobPreVerifyResult =
                        preVerifyREdDSA
                          bobPubKey
                          bobTxHash
                          adapterPoint
                          bobPreSig
                          nizkProof

                  if not bobPreVerifyResult
                    then do
                      liftIO $ logError (partyName alice) "Bob's pre-signature verification failed"
                      return $ SwapFailure "Bob's pre-signature verification failed"
                    else do
                      liftIO $ logInfo (partyName alice) "Bob's pre-signature verified successfully"

                      -- Phase 5: Complete and publish Alice's signature
                      liftIO logSeparator
                      liftIO $ logSubPhase "Phase 5: Complete and Publish"

                      liftIO $
                        logAction (partyName alice) "Completing signature by adding adapter secret"
                      let aliceCompleteSig = adaptSignature alicePreSig adapterSecret

                      liftIO $
                        logInfo (partyName alice) "Signature completed (adapter secret now revealed)"

                      liftIO $ logAction (partyName alice) "Verifying completed signature"
                      let aliceSigVerifyResult =
                            verifyREdDSA (partyPublicKey alice) txHash aliceCompleteSig

                      if not aliceSigVerifyResult
                        then do
                          liftIO $ logError (partyName alice) "Completed signature verification failed"
                          return $ SwapFailure "Alice's completed signature verification failed"
                        else do
                          liftIO $ logInfo (partyName alice) "Completed signature verified successfully"

                          -- Create signed transaction
                          let signedAliceTx = aliceTx {txSignatures = [aliceCompleteSig]}

                          liftIO $ logAction (partyName alice) "Publishing transaction to ChainA"
                          submitResult <- submitTransaction chainA signedAliceTx

                          case submitResult of
                            Left err -> do
                              liftIO $ logError (partyName alice) ("Transaction submission failed: " <> err)
                              return $ SwapFailure ("Alice's transaction submission failed: " <> err)
                            Right txId -> do
                              let TxId txIdBytes = txId
                              liftIO $
                                logTransaction (partyName alice) "Transaction published successfully" txIdBytes

                              -- Notify Bob that swap is complete
                              liftIO $
                                logAction (partyName alice) "Notifying Bob that transaction is published"
                              sendMessage toBob SwapCompleteMsg

                              -- Final status
                              liftIO logSeparator
                              liftIO $ logPhase "ALICE'S PROTOCOL COMPLETE"

                              finalBalance <- getBalance chainA (partyPublicKey alice)
                              liftIO $
                                logInfo
                                  (partyName alice)
                                  ("Final balance on ChainA: " <> show finalBalance)

                              liftIO $ logInfo (partyName alice) "✓ Atomic swap completed successfully"
                              liftIO $
                                logInfo
                                  (partyName alice)
                                  "✓ Adapter secret revealed - Bob can now complete his transaction"

                              return SwapSuccess
                _ -> do
                  liftIO $ logError (partyName alice) "Expected TransactionProposalMsg from Bob"
                  return $ SwapFailure "Protocol error: Expected transaction proposal from Bob"
