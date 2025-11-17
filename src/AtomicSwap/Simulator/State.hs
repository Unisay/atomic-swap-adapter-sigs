{-# LANGUAGE StrictData #-}

{- |
Module: AtomicSwap.Simulator.State
Description: State management for atomic swap simulator

This module provides state management using an append-only log with
fold-based reconstruction for time-travel debugging capabilities.
-}
module AtomicSwap.Simulator.State
  ( -- * State Management
    SimulatorState (..)
  , PartyState (..)
  , emptySimulatorState
  , emptyPartyState
  , appendStep
  , getPartyState
  , getStepCount

    -- * State Reconstruction
  , applyUpdate
  , reconstructState
  ) where

import Prelude hiding (Maybe, Seq, state)

import AtomicSwap.Prelude hiding (Party)
import AtomicSwap.Simulator.Types
import AtomicSwap.Simulator.Types.Orphans ()
import Control.Foldl qualified as L
import Data.Strict.Maybe qualified as SM
import Data.Strict.Sequence qualified as Seq
import NoThunks.Class (NoThunks (..))

--------------------------------------------------------------------------------
-- Party State (Reconstructed from Updates) ------------------------------------

-- | Ephemeral party state computed from updates (fully strict via StrictData)
data PartyState = PartyState
  { psPrivateKey :: SM.Maybe Ed25519PrivateKey
  , psPublicKey :: SM.Maybe PublicKey
  , psAdapterSecret :: SM.Maybe AdapterSecret
  , psAdapterCommitment :: SM.Maybe AdapterPoint
  , psPreSignature :: SM.Maybe Signature
  , psObservedTxs :: Seq.Seq TxId
  , psIsWaiting :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

instance NoThunks PartyState

emptyPartyState :: PartyState
emptyPartyState =
  force $
    PartyState
      { psPrivateKey = SM.Nothing
      , psPublicKey = SM.Nothing
      , psAdapterSecret = SM.Nothing
      , psAdapterCommitment = SM.Nothing
      , psPreSignature = SM.Nothing
      , psObservedTxs = Seq.empty
      , psIsWaiting = False
      }

--------------------------------------------------------------------------------
-- Simulator State (Append-Only Log) ------------------------------------------

-- | Complete simulator state
data SimulatorState = SimulatorState
  { ssGlobalState :: GlobalState
  , ssAliceState :: PartyState
  , ssBobState :: PartyState
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

instance NoThunks SimulatorState

emptySimulatorState :: SimulatorState
emptySimulatorState =
  force $
    SimulatorState
      { ssGlobalState = []
      , ssAliceState = emptyPartyState
      , ssBobState = emptyPartyState
      }

--------------------------------------------------------------------------------
-- State Operations ------------------------------------------------------------

-- | Append a new step to the global state
appendStep
  :: Participant -> UserInputs -> [StateUpdate] -> SimulatorState -> SimulatorState
appendStep participant inputs updates state =
  let stepIndex = StepIndex $ length (ssGlobalState state)
      entry = (stepIndex, participant, inputs, updates)
      newGlobalState = ssGlobalState state <> [entry]
   in reconstructState newGlobalState

-- | Get party state
getPartyState :: Participant -> SimulatorState -> PartyState
getPartyState Alice = ssAliceState
getPartyState Bob = ssBobState

-- | Get current step count
getStepCount :: SimulatorState -> Int
getStepCount = length . ssGlobalState

--------------------------------------------------------------------------------
-- State Reconstruction --------------------------------------------------------

-- | Apply a single state update to party state
applyUpdate :: Participant -> StateUpdate -> PartyState -> PartyState
applyUpdate party update partyState = case update of
  SetPrivateKey p sk
    | p == party ->
        partyState {psPrivateKey = SM.Just sk}
  SetPublicKey p pk
    | p == party ->
        partyState {psPublicKey = SM.Just pk}
  SetAdapterSecret p secret
    | p == party ->
        partyState {psAdapterSecret = SM.Just secret}
  SetAdapterCommitment p commitment
    | p == party ->
        partyState {psAdapterCommitment = SM.Just commitment}
  SetPreSignature p sig
    | p == party ->
        partyState {psPreSignature = SM.Just sig}
  SetThreadWaiting p waiting
    | p == party ->
        partyState {psIsWaiting = waiting}
  ObserveTransaction p _ txId
    | p == party ->
        partyState {psObservedTxs = psObservedTxs partyState Seq.:|> txId}
  _ -> partyState -- Ignore updates not relevant to this party

-- | Fold for reconstructing Alice's state
aliceStateFold :: L.Fold StepEntry PartyState
aliceStateFold = L.Fold step emptyPartyState id
  where
    step :: PartyState -> StepEntry -> PartyState
    step partyState (_idx, _participant, _inputs, updates) =
      foldl' (flip $ applyUpdate Alice) partyState updates

-- | Fold for reconstructing Bob's state
bobStateFold :: L.Fold StepEntry PartyState
bobStateFold = L.Fold step emptyPartyState id
  where
    step :: PartyState -> StepEntry -> PartyState
    step partyState (_idx, _participant, _inputs, updates) =
      foldl' (flip $ applyUpdate Bob) partyState updates

-- | Reconstruct complete simulator state from global state
reconstructState :: GlobalState -> SimulatorState
reconstructState globalState =
  SimulatorState
    { ssGlobalState = globalState
    , ssAliceState = L.fold aliceStateFold globalState
    , ssBobState = L.fold bobStateFold globalState
    }
