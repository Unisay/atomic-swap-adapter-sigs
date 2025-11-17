{-# LANGUAGE StrictData #-}

{- |
Module: AtomicSwap.Simulator.Steps
Description: Step execution handlers for atomic swap simulator

This module provides pure step execution logic separated from HTTP concerns.
Each executor takes inputs via MonadSimulator operations and returns view data.

= State Management Pattern

All step executors follow the Event Sourcing pattern:

1. Read current state via 'getPartyState' (read-only projection)
2. Generate new cryptographic values
3. Call 'applyUpdates' to append StateUpdates to GlobalState
4. Read updated state projection via 'getPartyState'
5. Return 'RenderContext' with the acting participant's updated state

The 'applyUpdates' participant parameter indicates WHO performed the action,
while the StateUpdate list can contain changes for ANY participant(s).
Both party states are automatically recomputed from the append-only log.
-}
module AtomicSwap.Simulator.Steps
  ( -- * Step Executors
    executeAliceKeygen
  , executeBobKeygen
  , executeAliceGenerateSecret
  , executeAliceMakeCommitment
  , executeAliceSendPublicKey
  , executeBobSendPublicKey
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
-- Step Executors --------------------------------------------------------------

-- | Execute Alice's keypair generation step
executeAliceKeygen :: MonadSimulator m => m ()
executeAliceKeygen = do
  (sk, pk) <- generateKeyPair
  applyUpdates
    Alice
    (UserInputs "")
    [SetPrivateKey Alice sk, SetPublicKey Alice pk]

-- | Execute Bob's keypair generation step
executeBobKeygen :: MonadSimulator m => m ()
executeBobKeygen = do
  (sk, pk) <- generateKeyPair
  applyUpdates Bob (UserInputs "") [SetPrivateKey Bob sk, SetPublicKey Bob pk]

-- | Execute Alice's adapter secret generation step
executeAliceGenerateSecret :: MonadSimulator m => m ()
executeAliceGenerateSecret = do
  secret <- generateAdapterSecret
  applyUpdates Alice (UserInputs "") [SetAdapterSecret Alice secret]

-- | Execute Alice's commitment generation step
executeAliceMakeCommitment :: MonadSimulator m => m Bool
executeAliceMakeCommitment = do
  aliceState <- getPartyState Alice
  case psAdapterSecret aliceState of
    SM.Nothing -> pure False -- Precondition not met
    SM.Just secret -> do
      commitment <- generateAdapterCommitment secret
      applyUpdates Alice (UserInputs "") [SetAdapterCommitment Alice commitment]
      pure True

-- | Execute Alice sending her public key to Bob
executeAliceSendPublicKey :: MonadSimulator m => m Bool
executeAliceSendPublicKey = do
  -- 1. Read current state (read-only projection)
  aliceState <- getPartyState Alice
  case psPublicKey aliceState of
    SM.Nothing -> pure False -- Alice hasn't generated keys yet
    SM.Just alicePk -> do
      -- 2. Append updates to GlobalState (Alice is the actor)
      --    Updates affect BOTH participants (Bob receives, Alice marks sent)
      --    State-diffing will automatically detect both parties changed
      applyUpdates
        Alice -- Actor who initiated this step
        (UserInputs "")
        [ SetOtherPartyPublicKey Bob alicePk -- Bob's state changes
        , SetSentPublicKey Alice True -- Alice's state changes
        ]
      pure True

-- | Execute Bob sending his public key to Alice
executeBobSendPublicKey :: MonadSimulator m => m Bool
executeBobSendPublicKey = do
  bobState <- getPartyState Bob
  case psPublicKey bobState of
    SM.Nothing -> pure False -- Bob hasn't generated keys yet
    SM.Just bobPk -> do
      -- Alice receives Bob's public key, Bob marks as sent (cross-participant update)
      -- State-diffing will automatically detect both parties changed
      applyUpdates
        Bob
        (UserInputs "")
        [ SetOtherPartyPublicKey Alice bobPk
        , SetSentPublicKey Bob True
        ]
      pure True
