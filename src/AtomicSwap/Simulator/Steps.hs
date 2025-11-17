{-# LANGUAGE StrictData #-}

{- |
Module: AtomicSwap.Simulator.Steps
Description: Step execution handlers for atomic swap simulator

This module provides pure step execution logic separated from HTTP concerns.
Each executor takes inputs via MonadSimulator operations and returns view data.
-}
module AtomicSwap.Simulator.Steps
  ( -- * Render Context
    RenderContext (..)

    -- * Step Executors
  , executeAliceKeygen
  , executeBobKeygen
  , executeAliceGenerateSecret
  , executeAliceMakeCommitment
  ) where

import Prelude

import AtomicSwap.Simulator.Class (MonadSimulator (..))
import AtomicSwap.Simulator.State (PartyState (..))
import AtomicSwap.Simulator.Types
  ( Participant (..)
  , StateUpdate (..)
  , UserInputs (..)
  )
import Data.Strict.Maybe qualified as SM

--------------------------------------------------------------------------------
-- Render Context --------------------------------------------------------------

-- | Context for rendering UI (ELM-style: full state + metadata)
data RenderContext = RenderContext
  { rcStepNum :: Int
  , rcParticipant :: Participant
  , rcPartyState :: PartyState
  }
  deriving stock (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- Step Executors --------------------------------------------------------------

-- | Execute Alice's keypair generation step
executeAliceKeygen :: MonadSimulator m => m RenderContext
executeAliceKeygen = do
  (sk, pk) <- generateKeyPair
  applyUpdates
    Alice
    (UserInputs "")
    [SetPrivateKey Alice sk, SetPublicKey Alice pk]
  stepNum <- getStepCount
  aliceState <- getPartyState Alice
  pure $ RenderContext stepNum Alice aliceState

-- | Execute Bob's keypair generation step
executeBobKeygen :: MonadSimulator m => m RenderContext
executeBobKeygen = do
  (sk, pk) <- generateKeyPair
  applyUpdates Bob (UserInputs "") [SetPrivateKey Bob sk, SetPublicKey Bob pk]
  stepNum <- getStepCount
  bobState <- getPartyState Bob
  pure $ RenderContext stepNum Bob bobState

-- | Execute Alice's adapter secret generation step
executeAliceGenerateSecret :: MonadSimulator m => m RenderContext
executeAliceGenerateSecret = do
  secret <- generateAdapterSecret
  applyUpdates Alice (UserInputs "") [SetAdapterSecret Alice secret]
  stepNum <- getStepCount
  aliceState <- getPartyState Alice
  pure $ RenderContext stepNum Alice aliceState

-- | Execute Alice's commitment generation step
executeAliceMakeCommitment :: MonadSimulator m => m (Maybe RenderContext)
executeAliceMakeCommitment = do
  aliceState <- getPartyState Alice
  case psAdapterSecret aliceState of
    SM.Nothing -> pure Nothing -- Precondition not met
    SM.Just secret -> do
      commitment <- generateAdapterCommitment secret
      applyUpdates Alice (UserInputs "") [SetAdapterCommitment Alice commitment]
      stepNum <- getStepCount
      aliceState' <- getPartyState Alice
      pure $ Just $ RenderContext stepNum Alice aliceState'
