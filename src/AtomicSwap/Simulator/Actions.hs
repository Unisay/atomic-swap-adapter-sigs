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
  , BobVerifyAlicePreSignature
  , BobPrepareTransaction
  , BobCreatePreSignature
  , BobPublishPreSignature
  , AliceVerifyBobPreSignature
  , AliceCompleteSignature
  , BobExtractSecret
  , BobCompleteSignature
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
        BobVerifyAlicePreSignature ->
          SM.isJust (psOtherPartyTransaction bob)
            && SM.isJust (psOtherPartyPreSignature bob)
            && not (psPreSignatureVerified bob)
        BobPrepareTransaction ->
          psPreSignatureVerified bob && SM.isNothing (psTransaction bob)
        BobCreatePreSignature ->
          SM.isJust (psTransaction bob) && SM.isNothing (psPreSignature bob)
        BobPublishPreSignature ->
          SM.isJust (psPreSignature bob) && not (psSentPreSignature bob)
        AliceVerifyBobPreSignature ->
          SM.isJust (psOtherPartyTransaction alice)
            && SM.isJust (psOtherPartyPreSignature alice)
            && not (psPreSignatureVerified alice)
        AliceCompleteSignature ->
          psPreSignatureVerified alice && SM.isNothing (psCompleteSignature alice)
        BobExtractSecret ->
          SM.isJust (psOtherPartyCompleteSignature bob)
            && SM.isNothing (psExtractedSecret bob)
        BobCompleteSignature ->
          SM.isJust (psExtractedSecret bob) && SM.isNothing (psCompleteSignature bob)

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
  BobVerifyAlicePreSignature ->
    ActionMetadata
      Bob
      "Verify Alice's Pre-Sig"
      "/step/bob-verify-alice-pre-signature"
  BobPrepareTransaction ->
    ActionMetadata Bob "Prepare Tx" "/step/bob-prepare-transaction"
  BobCreatePreSignature ->
    ActionMetadata Bob "Create Pre-Sig" "/step/bob-create-pre-signature"
  BobPublishPreSignature ->
    ActionMetadata Bob "Publish Pre-Sig" "/step/bob-publish-pre-signature"
  AliceVerifyBobPreSignature ->
    ActionMetadata
      Alice
      "Verify Bob's Pre-Sig"
      "/step/alice-verify-bob-pre-signature"
  AliceCompleteSignature ->
    ActionMetadata Alice "Complete & Publish Sig" "/step/alice-complete-signature"
  BobExtractSecret ->
    ActionMetadata Bob "Extract Secret" "/step/bob-extract-secret"
  BobCompleteSignature ->
    ActionMetadata Bob "Complete & Publish Sig" "/step/bob-complete-signature"
