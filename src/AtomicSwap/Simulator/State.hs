{-# LANGUAGE StrictData #-}

{- |
Module: AtomicSwap.Simulator.State
Description: State management for atomic swap simulator

This module provides state management using an append-only log with
fold-based reconstruction for time-travel debugging capabilities.

= Architecture

The design follows Event Sourcing principles:

* 'GlobalState' - Append-only log of state updates (source of truth)
* 'PartyState' - Read-only projections computed by folding GlobalState
* 'appendStep' - Only way to modify state (appends to GlobalState)
* 'reconstructState' - Rebuilds both party states from GlobalState

This ensures time-travel debugging and deterministic state reconstruction.
-}
module AtomicSwap.Simulator.State
  ( -- * State Management
    SimulatorState (..)
  , PartyState (..)
  , mkSimulatorState
  , emptySimulatorState
  , emptyPartyState
  , appendStep
  , getPartyState
  , getStepCount

    -- * State Reconstruction
  , applyUpdate
  , reconstructState

    -- * State Diffing
  , detectChangedParties
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

{- | Ephemeral party state computed from updates (fully strict via StrictData)

This is a READ-ONLY projection. Never modify directly - always append to
GlobalState via 'appendStep', which will recompute both party states.
-}
data PartyState = PartyState
  { psPrivateKey :: SM.Maybe Ed25519PrivateKey
  , psPublicKey :: SM.Maybe PublicKey
  , psOtherPartyPublicKey :: SM.Maybe PublicKey
  , psSentPublicKey :: Bool
  , psAdapterSecret :: SM.Maybe AdapterSecret
  , psAdapterCommitment :: SM.Maybe AdapterPoint
  , psSentCommitment :: Bool
  , psOtherPartyCommitment :: SM.Maybe AdapterPoint
  , psNIZKProof :: SM.Maybe NIZKProof
  , psSentNIZKProof :: Bool
  , psOtherPartyNIZKProof :: SM.Maybe NIZKProof
  , psNIZKProofVerified :: Bool
  , psTransaction :: SM.Maybe Transaction
  , psPreSignature :: SM.Maybe AdaptedSignature
  , psSentPreSignature :: Bool
  , psOtherPartyTransaction :: SM.Maybe Transaction
  , psOtherPartyPreSignature :: SM.Maybe AdaptedSignature
  , psPreSignatureVerified :: Bool
  , psCompleteSignature :: SM.Maybe Signature
  , psOtherPartyCompleteSignature :: SM.Maybe Signature
  , psExtractedSecret :: SM.Maybe AdapterSecret
  , psObservedTxs :: Seq.Seq TxId
  , psIsWaiting :: Bool
  , -- Fresh flags (True if field was updated in most recent step)
    psPrivateKeyFresh :: Bool
  , psPublicKeyFresh :: Bool
  , psOtherPartyPublicKeyFresh :: Bool
  , psAdapterSecretFresh :: Bool
  , psAdapterCommitmentFresh :: Bool
  , psNIZKProofFresh :: Bool
  , psNIZKProofVerifiedFresh :: Bool
  , psTransactionFresh :: Bool
  , psPreSignatureFresh :: Bool
  , psOtherPartyTransactionFresh :: Bool
  , psOtherPartyPreSignatureFresh :: Bool
  , psPreSignatureVerifiedFresh :: Bool
  , psCompleteSignatureFresh :: Bool
  , psOtherPartyCompleteSignatureFresh :: Bool
  , psExtractedSecretFresh :: Bool
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
      , psOtherPartyPublicKey = SM.Nothing
      , psSentPublicKey = False
      , psAdapterSecret = SM.Nothing
      , psAdapterCommitment = SM.Nothing
      , psSentCommitment = False
      , psOtherPartyCommitment = SM.Nothing
      , psNIZKProof = SM.Nothing
      , psSentNIZKProof = False
      , psOtherPartyNIZKProof = SM.Nothing
      , psNIZKProofVerified = False
      , psTransaction = SM.Nothing
      , psPreSignature = SM.Nothing
      , psSentPreSignature = False
      , psOtherPartyTransaction = SM.Nothing
      , psOtherPartyPreSignature = SM.Nothing
      , psPreSignatureVerified = False
      , psCompleteSignature = SM.Nothing
      , psOtherPartyCompleteSignature = SM.Nothing
      , psExtractedSecret = SM.Nothing
      , psObservedTxs = Seq.empty
      , psIsWaiting = False
      , -- Fresh flags
        psPrivateKeyFresh = False
      , psPublicKeyFresh = False
      , psOtherPartyPublicKeyFresh = False
      , psAdapterSecretFresh = False
      , psAdapterCommitmentFresh = False
      , psNIZKProofFresh = False
      , psNIZKProofVerifiedFresh = False
      , psTransactionFresh = False
      , psPreSignatureFresh = False
      , psOtherPartyTransactionFresh = False
      , psOtherPartyPreSignatureFresh = False
      , psPreSignatureVerifiedFresh = False
      , psCompleteSignatureFresh = False
      , psOtherPartyCompleteSignatureFresh = False
      , psExtractedSecretFresh = False
      }

--------------------------------------------------------------------------------
-- Simulator State (Append-Only Log) ------------------------------------------

-- | Complete simulator state
data SimulatorState = SimulatorState
  { ssGlobalState :: GlobalState
  , ssAliceState :: PartyState
  , ssBobState :: PartyState
  , ssSwapAmounts
      :: ( Quantity 'Apple
         , Quantity 'Banana
         )
  -- ^ (apples from Alice, bananas from Bob)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

instance NoThunks SimulatorState

-- | Create initial simulator state with agreed swap amounts
mkSimulatorState :: Quantity 'Apple -> Quantity 'Banana -> SimulatorState
mkSimulatorState applesFromAlice bananasFromBob =
  force $
    SimulatorState
      { ssGlobalState = []
      , ssAliceState = emptyPartyState
      , ssBobState = emptyPartyState
      , ssSwapAmounts = (applesFromAlice, bananasFromBob)
      }

-- | Deprecated: Use mkSimulatorState instead
emptySimulatorState :: SimulatorState
emptySimulatorState = mkSimulatorState 0 0

--------------------------------------------------------------------------------
-- State Operations ------------------------------------------------------------

{- | Append a new step to the global state (ONLY way to modify state)

This is the sole mutation point. It:
1. Appends a new entry to GlobalState (append-only log)
2. Reconstructs BOTH party states from the complete GlobalState
3. Returns new SimulatorState with updated projections

The 'Participant' parameter indicates which party initiated the action,
but the updates list can contain changes for ANY participant.
-}
appendStep
  :: Participant -> UserInputs -> [StateUpdate] -> SimulatorState -> SimulatorState
appendStep participant inputs updates state =
  let stepIndex = StepIndex $ length (ssGlobalState state)
      entry = (stepIndex, participant, inputs, updates)
      newGlobalState = ssGlobalState state <> [entry]
   in reconstructState (ssSwapAmounts state) newGlobalState

-- | Get party state
getPartyState :: Participant -> SimulatorState -> PartyState
getPartyState Alice = ssAliceState
getPartyState Bob = ssBobState

-- | Get current step count
getStepCount :: SimulatorState -> Int
getStepCount = length . ssGlobalState

--------------------------------------------------------------------------------
-- State Reconstruction --------------------------------------------------------

-- | Mark fields as fresh based on recent updates (called after reconstruction)
markFreshFields :: Participant -> [StateUpdate] -> PartyState -> PartyState
markFreshFields party updates state =
  state
    { psPrivateKeyFresh =
        any (\case SetPrivateKey p _ -> p == party; _ -> False) updates
    , psPublicKeyFresh =
        any (\case SetPublicKey p _ -> p == party; _ -> False) updates
    , psOtherPartyPublicKeyFresh =
        any (\case SetOtherPartyPublicKey p _ -> p == party; _ -> False) updates
    , psAdapterSecretFresh =
        any (\case SetAdapterSecret p _ -> p == party; _ -> False) updates
    , psAdapterCommitmentFresh =
        any (\case SetAdapterCommitment p _ -> p == party; _ -> False) updates
    , psNIZKProofFresh =
        any (\case SetNIZKProof p _ -> p == party; _ -> False) updates
    , psNIZKProofVerifiedFresh =
        any (\case SetNIZKProofVerified p _ -> p == party; _ -> False) updates
    , psTransactionFresh =
        any (\case SetTransaction p _ -> p == party; _ -> False) updates
    , psPreSignatureFresh =
        any (\case SetPreSignature p _ -> p == party; _ -> False) updates
    , psOtherPartyTransactionFresh =
        any (\case SetOtherPartyTransaction p _ -> p == party; _ -> False) updates
    , psOtherPartyPreSignatureFresh =
        any (\case SetOtherPartyPreSignature p _ -> p == party; _ -> False) updates
    , psPreSignatureVerifiedFresh =
        any (\case SetPreSignatureVerified p _ -> p == party; _ -> False) updates
    , psCompleteSignatureFresh =
        any (\case SetCompleteSignature p _ -> p == party; _ -> False) updates
    , psOtherPartyCompleteSignatureFresh =
        any (\case SetOtherPartyCompleteSignature p _ -> p == party; _ -> False) updates
    , psExtractedSecretFresh =
        any (\case SetExtractedSecret p _ -> p == party; _ -> False) updates
    }

-- | Apply a single state update to party state (does not set fresh flags)
applyUpdate :: Participant -> StateUpdate -> PartyState -> PartyState
applyUpdate party update partyState = case update of
  SetPrivateKey p sk
    | p == party ->
        partyState {psPrivateKey = SM.Just sk}
  SetPublicKey p pk
    | p == party ->
        partyState {psPublicKey = SM.Just pk}
  SetOtherPartyPublicKey p pk
    | p == party ->
        partyState {psOtherPartyPublicKey = SM.Just pk}
  SetSentPublicKey p sent
    | p == party ->
        partyState {psSentPublicKey = sent}
  SetAdapterSecret p secret
    | p == party ->
        partyState {psAdapterSecret = SM.Just secret}
  SetAdapterCommitment p commitment
    | p == party ->
        partyState {psAdapterCommitment = SM.Just commitment}
  SetSentCommitment p sent
    | p == party ->
        partyState {psSentCommitment = sent}
  SetOtherPartyCommitment p commitment
    | p == party ->
        partyState {psOtherPartyCommitment = SM.Just commitment}
  SetNIZKProof p proof
    | p == party ->
        partyState {psNIZKProof = SM.Just proof}
  SetSentNIZKProof p sent
    | p == party ->
        partyState {psSentNIZKProof = sent}
  SetOtherPartyNIZKProof p proof
    | p == party ->
        partyState {psOtherPartyNIZKProof = SM.Just proof}
  SetNIZKProofVerified p verified
    | p == party ->
        partyState {psNIZKProofVerified = verified}
  SetTransaction p tx
    | p == party ->
        partyState {psTransaction = SM.Just tx}
  SetPreSignature p sig
    | p == party ->
        partyState {psPreSignature = SM.Just sig}
  SetSentPreSignature p sent
    | p == party ->
        partyState {psSentPreSignature = sent}
  SetOtherPartyTransaction p tx
    | p == party ->
        partyState {psOtherPartyTransaction = SM.Just tx}
  SetOtherPartyPreSignature p sig
    | p == party ->
        partyState {psOtherPartyPreSignature = SM.Just sig}
  SetPreSignatureVerified p verified
    | p == party ->
        partyState {psPreSignatureVerified = verified}
  SetCompleteSignature p sig
    | p == party ->
        partyState {psCompleteSignature = SM.Just sig}
  SetOtherPartyCompleteSignature p sig
    | p == party ->
        partyState {psOtherPartyCompleteSignature = SM.Just sig}
  SetExtractedSecret p secret
    | p == party ->
        partyState {psExtractedSecret = SM.Just secret}
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
reconstructState
  :: (Quantity 'Apple, Quantity 'Banana) -> GlobalState -> SimulatorState
reconstructState swapAmounts globalState =
  let
    -- Reconstruct states from full history
    baseAliceState = L.fold aliceStateFold globalState
    baseBobState = L.fold bobStateFold globalState
    -- Mark fresh fields only from the most recent step
    (aliceState, bobState) = case viaNonEmpty last globalState of
      Nothing -> (baseAliceState, baseBobState)
      Just (_idx, _participant, _inputs, updates) ->
        ( markFreshFields Alice updates baseAliceState
        , markFreshFields Bob updates baseBobState
        )
   in
    SimulatorState
      { ssGlobalState = globalState
      , ssAliceState = aliceState
      , ssBobState = bobState
      , ssSwapAmounts = swapAmounts
      }

--------------------------------------------------------------------------------
-- State Diffing ---------------------------------------------------------------

{- | Detect which parties' states changed between two simulator states

Used for automatic UI updates: only render panels for parties whose state
actually changed after a step execution.
-}
detectChangedParties :: SimulatorState -> SimulatorState -> [Participant]
detectChangedParties oldState newState =
  let aliceChanged = ssAliceState oldState /= ssAliceState newState
      bobChanged = ssBobState oldState /= ssBobState newState
   in [Alice | aliceChanged] <> [Bob | bobChanged]
