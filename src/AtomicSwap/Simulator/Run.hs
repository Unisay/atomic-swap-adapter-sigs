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

import AtomicSwap.Crypto.Adapter qualified as Adapter
import AtomicSwap.Crypto.Keys qualified as Keys
import AtomicSwap.Simulator.Class (MonadSimulator (..))
import AtomicSwap.Simulator.State (SimulatorState, appendStep)
import AtomicSwap.Simulator.State qualified as State
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

  applyUpdates participant inputs updates = SimulatorT $ ReaderT \ref ->
    Strict.modifyIORef' ref $ appendStep participant inputs updates

  generateKeyPair = SimulatorT $ ReaderT \_ ->
    Keys.generateKeyPair

  generateAdapterSecret = SimulatorT $ ReaderT \_ ->
    Adapter.generateAdapterSecret

  generateAdapterCommitment secret = SimulatorT $ ReaderT \_ ->
    pure $ Adapter.generateAdapterCommitment secret
