{-# LANGUAGE StrictData #-}

{- |
Module: AtomicSwap.Simulator.Actions
Description: Business logic for determining available and recommended actions

This module separates action availability logic from presentation.
Pure functions determine which actions can be performed based on state.
-}
module AtomicSwap.Simulator.Actions
  ( -- * Protocol Order
    protocolOrder

    -- * Action Availability
  , availableActions
  , recommendedAction
  , actionMetadata
  , ActionMetadata (..)
  ) where

import Prelude

import AtomicSwap.Simulator.State (PartyState (..), SimulatorState (..))
import AtomicSwap.Simulator.State qualified as State
import AtomicSwap.Simulator.Types (Action (..), Participant (..))
import Data.Strict.Maybe qualified as SM

--------------------------------------------------------------------------------
-- Protocol Order (Canonical Sequence) -----------------------------------------

-- | Canonical protocol order (immutable, defines recommended sequence)
protocolOrder :: [Action]
protocolOrder =
  [ AliceKeygen
  , BobKeygen
  , AliceSendPublicKey
  , BobSendPublicKey
  , AliceGenerateSecret
  , AliceMakeCommitment
  , AliceSendCommitment
  , AliceGenerateNIZKProof
  , AliceSendNIZKProof
  , BobVerifyNIZKProof
  , AlicePrepareTransaction
  , AliceCreatePreSignature
  , AlicePublishPreSignature
  ]

--------------------------------------------------------------------------------
-- Action Availability Logic ---------------------------------------------------

-- | Check if an action can be performed in current state
canPerformAction :: SimulatorState -> Action -> Bool
canPerformAction simState action =
  let alice = State.getPartyState Alice simState
      bob = State.getPartyState Bob simState
   in case action of
        AliceKeygen -> SM.isNothing (psPublicKey alice)
        BobKeygen -> SM.isNothing (psPublicKey bob)
        AliceSendPublicKey ->
          SM.isJust (psPublicKey alice) && not (psSentPublicKey alice)
        BobSendPublicKey ->
          SM.isJust (psPublicKey bob) && not (psSentPublicKey bob)
        AliceGenerateSecret ->
          SM.isNothing (psAdapterSecret alice)
        AliceMakeCommitment ->
          SM.isJust (psAdapterSecret alice) && SM.isNothing (psAdapterCommitment alice)
        AliceSendCommitment ->
          SM.isJust (psAdapterCommitment alice) && not (psSentCommitment alice)
        AliceGenerateNIZKProof ->
          psSentCommitment alice && SM.isNothing (psNIZKProof alice)
        AliceSendNIZKProof ->
          SM.isJust (psNIZKProof alice) && not (psSentNIZKProof alice)
        BobVerifyNIZKProof ->
          SM.isJust (psOtherPartyCommitment bob)
            && SM.isJust (psOtherPartyNIZKProof bob)
            && not (psNIZKProofVerified bob)
        AlicePrepareTransaction ->
          SM.isJust (psOtherPartyPublicKey alice)
            && SM.isNothing (psTransaction alice)
        AliceCreatePreSignature ->
          SM.isJust (psTransaction alice) && SM.isNothing (psPreSignature alice)
        AlicePublishPreSignature ->
          SM.isJust (psPreSignature alice) && not (psSentPreSignature alice)

-- | Get all currently available actions (in protocol order)
availableActions :: SimulatorState -> [Action]
availableActions simState = filter (canPerformAction simState) protocolOrder

-- | Get recommended next action (first available in protocol order)
recommendedAction :: SimulatorState -> Maybe Action
recommendedAction simState = viaNonEmpty head (availableActions simState)

--------------------------------------------------------------------------------
-- Action Metadata -------------------------------------------------------------

-- | Metadata for rendering an action
data ActionMetadata = ActionMetadata
  { amParticipant :: Participant
  , amButtonText :: Text
  , amEndpoint :: Text
  }
  deriving stock (Eq, Show)

-- | Get display metadata for an action
actionMetadata :: Action -> ActionMetadata
actionMetadata = \case
  AliceKeygen ->
    ActionMetadata Alice "Generate Keys" "/step/alice-keygen"
  BobKeygen ->
    ActionMetadata Bob "Generate Keys" "/step/bob-keygen"
  AliceSendPublicKey ->
    ActionMetadata Alice "Publish Pubkey" "/step/alice-send-public-key"
  BobSendPublicKey ->
    ActionMetadata Bob "Publish Pubkey" "/step/bob-send-public-key"
  AliceGenerateSecret ->
    ActionMetadata Alice "Generate Secret" "/step/alice-generate-secret"
  AliceMakeCommitment ->
    ActionMetadata Alice "Seal Secret" "/step/alice-make-commitment"
  AliceSendCommitment ->
    ActionMetadata Alice "Publish Seal" "/step/alice-send-commitment"
  AliceGenerateNIZKProof ->
    ActionMetadata Alice "Prove Seal" "/step/alice-generate-nizk-proof"
  AliceSendNIZKProof ->
    ActionMetadata Alice "Publish Proof" "/step/alice-send-nizk-proof"
  BobVerifyNIZKProof ->
    ActionMetadata Bob "Verify Proof" "/step/bob-verify-nizk-proof"
  AlicePrepareTransaction ->
    ActionMetadata Alice "Prepare Tx" "/step/alice-prepare-transaction"
  AliceCreatePreSignature ->
    ActionMetadata Alice "Create Pre-Sig" "/step/alice-create-pre-signature"
  AlicePublishPreSignature ->
    ActionMetadata Alice "Publish Pre-Sig" "/step/alice-publish-pre-signature"
