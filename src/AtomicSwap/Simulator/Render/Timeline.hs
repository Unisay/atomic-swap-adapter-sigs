{-# LANGUAGE StrictData #-}

{- |
Module: AtomicSwap.Simulator.Render.Timeline
Description: Timeline event rendering for simulator UI

Provides rendering functions for the protocol timeline including individual
step entries, step descriptions, and timeline navigation controls.
-}
module AtomicSwap.Simulator.Render.Timeline
  ( -- * Timeline Rendering
    renderTimeline
  , renderNewTimelineEntry

    -- * Step Hints
  , renderNextActionHint
  ) where

import Lucid
  ( Html
  , button_
  , class_
  , div_
  , id_
  , img_
  , span_
  , src_
  , style_
  , title_
  , toHtml
  )
import Prelude

import AtomicSwap.Simulator.Actions
  ( ActionMetadata (..)
  , actionMetadata
  , recommendedAction
  )
import AtomicSwap.Simulator.Render.Explanations
  ( getStepExplanation
  , renderExplanationPanel
  )
import AtomicSwap.Simulator.Render.Helpers
  ( getStepIcon
  , hxPost_
  , hxSwapOob_
  , hxSwap_
  , hxTarget_
  , stepIcon_
  )
import AtomicSwap.Simulator.State (SimulatorState (..))
import AtomicSwap.Simulator.Types
  ( Asset (..)
  , GlobalState
  , Participant (..)
  , Quantity (..)
  , StateUpdate (..)
  , StepEntry
  , StepIndex (..)
  )

--------------------------------------------------------------------------------
-- Timeline Rendering ----------------------------------------------------------

-- | Render the complete execution timeline from history
renderTimeline :: (Quantity 'Apple, Quantity 'Banana) -> GlobalState -> Html ()
renderTimeline (Quantity apples, Quantity bananas) globalState =
  case reverse globalState of
    [] -> renderStep0 apples bananas
    (lastStep : olderSteps) -> do
      -- Render last step (tip) WITHOUT reset button
      renderTimelineEntryWithoutReset lastStep
      -- Render older steps WITH reset buttons
      mapM_ renderTimelineEntry olderSteps
      -- Render Step 0 at bottom with reset button
      renderStep0 apples bananas

-- | Render step 0 (initial agreement) with reset button
renderStep0 :: Natural -> Natural -> Html ()
renderStep0 apples bananas =
  div_ [class_ "timeline-item"] do
    span_ [class_ "step-description"] $
      toHtml
        ( "Alice and Bob agree to exchange "
            <> show apples
            <> " 🍎 for "
            <> show bananas
            <> " 🍌"
            :: Text
        )
    -- Reset all button (visible on hover, resets entire protocol)
    button_
      [ class_ "reset-to-step-button"
      , hxPost_ "/reset"
      , hxTarget_ "body"
      , hxSwap_ "outerHTML"
      , title_ "Reset entire protocol"
      ]
      do img_ [src_ "/static/icons/refresh.svg"]
    span_ [class_ "step-number"] "0"

-- | Render hint for next recommended action (with optional OOB for updates)
renderNextActionHint :: SimulatorState -> Html ()
renderNextActionHint = renderNextActionHintWithOob False

renderNextActionHintWithOob :: Bool -> SimulatorState -> Html ()
renderNextActionHintWithOob useOob simState =
  case recommendedAction simState of
    Nothing ->
      div_
        ( [id_ "next-action-hint", class_ "next-action-hint complete"]
            <> [hxSwapOob_ "true" | useOob]
        )
        "✓ Protocol complete!"
    Just action ->
      let metadata = actionMetadata action
          participantName = case amParticipant metadata of
            Alice -> "Alice" :: Text
            Bob -> "Bob"
          buttonClass = case amParticipant metadata of
            Alice -> "party-button alice-button" :: Text
            Bob -> "party-button bob-button"
       in div_
            ( [id_ "next-action-hint", class_ "next-action-hint"]
                <> [hxSwapOob_ "true" | useOob]
            )
            do
              span_ [class_ "badge"] "Recommended"
              span_ [style_ "margin-right: 8px;"] $
                toHtml ("Next step: " <> participantName <> " should")
              button_
                [ class_ (buttonClass <> " next-action-button")
                , hxPost_ (amEndpoint metadata)
                , hxTarget_ "#timeline-entries"
                , hxSwap_ "afterbegin"
                ]
                (toHtml $ amButtonText metadata)

-- | Render a single timeline entry (with reset button)
renderTimelineEntry :: StepEntry -> Html ()
renderTimelineEntry entry@(StepIndex idx, _, _, _) =
  div_ [id_ ("step-" <> show idx)] do
    renderTimelineEntryContent entry True

-- | Render a single timeline entry WITHOUT reset button (for tip)
renderTimelineEntryWithoutReset :: StepEntry -> Html ()
renderTimelineEntryWithoutReset entry@(StepIndex idx, _, _, _) =
  div_ [id_ ("step-" <> show idx)] do
    renderTimelineEntryContent entry False

-- | Render timeline entry content (with or without reset button)
renderTimelineEntryContent :: StepEntry -> Bool -> Html ()
renderTimelineEntryContent
  (StepIndex idx, participant, _inputs, updates)
  includeReset =
    let eventClass = case participant of
          Alice -> "timeline-item alice-event" :: Text
          Bob -> "timeline-item bob-event" :: Text
        description = describeUpdates participant updates
        iconName = getStepIcon description
        stepId = show idx
        maybeExplanation = getStepExplanation stepId description
        -- Tip (no reset button) should have explanation open by default
        isOpenByDefault = not includeReset
     in div_ [class_ eventClass] do
          span_ [class_ "step-description"] do
            stepIcon_ iconName
            toHtml description
          -- Explanation panel (if available)
          for_ maybeExplanation $ \explanation ->
            renderExplanationPanel stepId explanation isOpenByDefault
          -- Reset button (conditionally rendered)
          when includeReset $
            button_
              [ class_ "reset-to-step-button"
              , hxPost_ ("/reset-to-step/" <> show idx)
              , hxTarget_ "body"
              , hxSwap_ "outerHTML"
              , title_ "Reset to this step"
              ]
              do img_ [src_ "/static/icons/refresh.svg"]
          span_ [class_ "step-number"] $ show (idx + 1)

-- | Generate human-readable description from state updates
describeUpdates :: Participant -> [StateUpdate] -> Text
describeUpdates participant updates =
  case updates of
    [SetPrivateKey _ _, SetPublicKey _ _] ->
      case participant of
        Alice -> "Alice generated Ed25519 keypair"
        Bob -> "Bob generated Ed25519 keypair"
    [SetOtherPartyPublicKey Bob _, SetSentPublicKey Alice _] ->
      "Alice shared her public key with Bob"
    [SetOtherPartyPublicKey Alice _, SetSentPublicKey Bob _] ->
      "Bob shared his public key with Alice"
    [SetAdapterSecret _ _] ->
      "Alice generated adapter secret y"
    [SetAdapterCommitment _ _] ->
      "Alice computed commitment Y = y·B"
    [SetOtherPartyCommitment Bob _, SetSentCommitment Alice _] ->
      "Alice shared commitment Y with Bob"
    [SetNIZKProof _ _] ->
      "Alice generated seal proof for commitment"
    [SetOtherPartyNIZKProof Bob _, SetSentNIZKProof Alice _] ->
      "Alice shared seal proof with Bob"
    [SetNIZKProofVerified Bob _] ->
      "Bob verified Alice's seal proof successfully"
    [SetTransaction Alice _] ->
      "Alice prepared transaction (apples → Bob)"
    [SetPreSignature Alice _] ->
      "Alice created adapter pre-signature"
    [ SetSentPreSignature Alice _
      , SetOtherPartyTransaction Bob _
      , SetOtherPartyPreSignature Bob _
      ] ->
        "Alice published transaction and pre-signature to Bob"
    [SetPreSignatureVerified Bob _] ->
      "Bob verified Alice's pre-signature successfully"
    [SetTransaction Bob _] ->
      "Bob prepared transaction (bananas → Alice)"
    [SetPreSignature Bob _] ->
      "Bob created adapter pre-signature"
    [ SetSentPreSignature Bob _
      , SetOtherPartyTransaction Alice _
      , SetOtherPartyPreSignature Alice _
      ] ->
        "Bob published transaction and pre-signature to Alice"
    [SetPreSignatureVerified Alice _] ->
      "Alice verified Bob's pre-signature successfully"
    [SetCompleteSignature Alice _, SetOtherPartyCompleteSignature Bob _] ->
      "Alice completed signature and published to blockchain (reveals secret y!)"
    [SetExtractedSecret Bob _] ->
      "Bob extracted adapter secret y from Alice's signature"
    [SetCompleteSignature Bob _] ->
      "Bob completed signature and published to blockchain"
    _ -> case participant of
      Alice -> "Alice performed action"
      Bob -> "Bob performed action"

--------------------------------------------------------------------------------
-- New Entry Rendering ---------------------------------------------------------

{- | Render new timeline entry from the latest step in GlobalState

Reads the last entry from GlobalState to create timeline event.
The new entry is the tip (no reset button), and we need to update
the previous tip to add its reset button via OOB swap.
-}
renderNewTimelineEntry :: SimulatorState -> Html ()
renderNewTimelineEntry simState = do
  let globalState = ssGlobalState simState
  -- Render new tip WITHOUT reset button
  for_ (viaNonEmpty last globalState) renderTimelineEntryWithoutReset
  -- Update previous tip to ADD reset button (if it exists)
  -- Get second-to-last element (previous tip)
  case viaNonEmpty init globalState of
    Just olderSteps -> for_ (viaNonEmpty last olderSteps) renderPreviousTipUpdate
    Nothing -> pure () -- Only one step, no previous tip
    -- Update hint with OOB swap (no need to delete first, OOB replaces it)
  renderNextActionHintWithOob True simState

-- | Render OOB update for previous tip (to add reset button)
renderPreviousTipUpdate :: StepEntry -> Html ()
renderPreviousTipUpdate entry@(StepIndex idx, _, _, _) =
  div_
    [ hxSwapOob_ ("outerHTML:#step-" <> show idx)
    , id_ ("step-" <> show idx)
    ]
    do renderTimelineEntryContent entry True
