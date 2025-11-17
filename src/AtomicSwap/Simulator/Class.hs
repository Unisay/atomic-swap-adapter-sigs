{-# LANGUAGE StrictData #-}

{- |
Module: AtomicSwap.Simulator.Class
Description: Abstraction for simulator operations

This module defines the MonadSimulator class which provides a clean
interface for step execution handlers, abstracting over state management
and cryptographic operations.
-}
module AtomicSwap.Simulator.Class
  ( MonadSimulator (..)
  ) where

import AtomicSwap.Prelude
import AtomicSwap.Simulator.State (PartyState)
import AtomicSwap.Simulator.Types (StateUpdate, UserInputs)

--------------------------------------------------------------------------------
-- MonadSimulator Class --------------------------------------------------------

-- | Abstraction for simulator operations used by step execution handlers
class Monad m => MonadSimulator m where
  -- | Get current step count (after all updates applied)
  getStepCount :: m Int

  -- | Get complete party state
  getPartyState :: Participant -> m PartyState

  -- | Apply state updates through the append-only log
  applyUpdates :: Participant -> UserInputs -> [StateUpdate] -> m ()

  -- | Generate Ed25519 keypair (delegated to crypto backend)
  generateKeyPair :: m (Ed25519PrivateKey, PublicKey)

  -- | Generate adapter secret (random scalar)
  generateAdapterSecret :: m AdapterSecret

  -- | Generate adapter commitment from secret (Y = y·B)
  generateAdapterCommitment :: AdapterSecret -> m AdapterPoint
