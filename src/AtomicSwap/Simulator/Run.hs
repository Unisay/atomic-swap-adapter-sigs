{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

{- HLINT ignore "Use const" -}

{- |
Module: AtomicSwap.Simulator.Run
Description: RIO-style simulator monad

This module provides the concrete SimulatorT monad using the RIO pattern:
a newtype around ReaderT (StrictIORef SimulatorState) IO.
-}
module AtomicSwap.Simulator.Run
  ( SimulatorT (..)
  , runSimulatorT
  ) where

import AtomicSwap.Blockchain.Transaction qualified as Tx
import AtomicSwap.Crypto.Adapter qualified as Adapter
import AtomicSwap.Crypto.Keys qualified as Keys
import AtomicSwap.Crypto.NIZK qualified as NIZK
import AtomicSwap.Simulator.Class (MonadSimulator (..))
import AtomicSwap.Simulator.State (SimulatorState, appendStep)
import AtomicSwap.Simulator.State qualified as State
import AtomicSwap.Types (Output (..), TxId (..), UTXO (..))
import Data.IORef.Strict (StrictIORef)
import Data.IORef.Strict qualified as Strict

--------------------------------------------------------------------------------
-- Simulator Monad -------------------------------------------------------------

newtype SimulatorT a = SimulatorT
  { unSimulatorT :: ReaderT (StrictIORef SimulatorState) IO a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

-- | Run a simulator computation with existing IORef
runSimulatorT :: StrictIORef SimulatorState -> SimulatorT a -> IO a
runSimulatorT ref (SimulatorT action) = runReaderT action ref

--------------------------------------------------------------------------------
-- MonadSimulator Instance -----------------------------------------------------

instance MonadSimulator SimulatorT where
  getStepCount = SimulatorT $ ReaderT \ref -> do
    simState <- Strict.readIORef ref
    pure $ State.getStepCount simState

  getPartyState participant = SimulatorT $ ReaderT \ref -> do
    simState <- Strict.readIORef ref
    pure $ State.getPartyState participant simState

  getSwapAmounts = SimulatorT $ ReaderT \ref -> do
    simState <- Strict.readIORef ref
    pure $ State.ssSwapAmounts simState

  applyUpdates participant inputs updates = SimulatorT $ ReaderT \ref ->
    Strict.modifyIORef' ref $ appendStep participant inputs updates

  generateKeyPair = SimulatorT $ ReaderT \_ ->
    Keys.generateKeyPair

  generateAdapterSecret = SimulatorT $ ReaderT \_ ->
    Adapter.generateAdapterSecret

  generateAdapterCommitment secret = SimulatorT $ ReaderT \_ ->
    pure $ Adapter.generateAdapterCommitment secret

  generateNIZKProof secret commitment = SimulatorT $ ReaderT \_ ->
    NIZK.proveDiscreteLog secret commitment

  verifyNIZKProof commitment proof = SimulatorT $ ReaderT \_ ->
    pure $ NIZK.verifyDiscreteLog commitment proof

  buildDummyTransaction recipientPk amount = SimulatorT $ ReaderT \_ -> do
    -- Create a dummy UTXO as input (represents available funds)
    let dummyTxId = TxId "0000000000000000000000000000000000000000000000000000000000000000"
        inputUTXO =
          UTXO
            { utxoTxId = dummyTxId
            , utxoIndex = 0
            , utxoAmount = fromIntegral amount
            , utxoOwner = recipientPk -- Will be overridden by signature
            }
        output =
          Output
            { outputAmount = fromIntegral amount
            , outputOwner = recipientPk
            }
    pure $ Tx.buildTransaction [inputUTXO] [output]

  createPreSignature privKey pubKey tx commitment proof =
    SimulatorT $ ReaderT \_ -> do
      let txHash = Tx.hashTransaction tx
      Adapter.preSignREdDSA privKey pubKey txHash commitment proof

  verifyPreSignature pubKey tx commitment preSig proof =
    SimulatorT $ ReaderT \_ -> do
      let txHash = Tx.hashTransaction tx
      pure $ Adapter.preVerifyREdDSA pubKey txHash commitment preSig proof

  completeSignature preSig secret = SimulatorT $ ReaderT \_ ->
    pure $ Adapter.adaptSignature preSig secret

  extractSecret preSig completeSig = SimulatorT $ ReaderT \_ ->
    pure $ Adapter.extractAdapterSecret preSig completeSig
