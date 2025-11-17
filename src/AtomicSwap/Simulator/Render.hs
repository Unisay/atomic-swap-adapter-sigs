{-# LANGUAGE StrictData #-}

{- |
Module: AtomicSwap.Simulator.Render
Description: HTML rendering for simulator UI (presentation layer)

This module provides HTML rendering functions that correspond to step executors.
Follows ELM architecture: renderers take full PartyState and render complete UI
using HTMX out-of-band updates for targeted DOM sections.

Correspondence with Steps module:
- executeAliceKeygen → renderStep with "Alice generated Ed25519 keypair"
- executeAliceGenerateSecret → renderStep with "Alice generated adapter secret y"
- executeAliceMakeCommitment → renderStep with "Alice computed commitment Y = y·B"
- executeBobKeygen → renderStep with "Bob generated Ed25519 keypair"
-}
module AtomicSwap.Simulator.Render
  ( -- * Page Templates
    mainPage

    -- * State-Diffing Renderers
  , renderStateUpdates
  , renderNewTimelineEntry
  ) where

import Lucid
  ( Attributes
  , Html
  , body_
  , button_
  , charset_
  , class_
  , content_
  , div_
  , doctypehtml_
  , h1_
  , h2_
  , h3_
  , head_
  , header_
  , href_
  , id_
  , link_
  , meta_
  , name_
  , p_
  , rel_
  , script_
  , span_
  , src_
  , style_
  , term
  , title_
  , toHtml
  )

import Data.ByteString.Base16 qualified as Base16
import Data.Strict.Maybe qualified as SM
import Data.Text.Encoding qualified as TE
import Prelude

import AtomicSwap.Simulator.Actions
  ( ActionMetadata (..)
  , actionMetadata
  , availableActions
  , recommendedAction
  )
import AtomicSwap.Simulator.State
  ( PartyState (..)
  , SimulatorState (..)
  , getPartyState
  )
import AtomicSwap.Simulator.Types
  ( Action
  , Asset (..)
  , GlobalState
  , Participant (..)
  , Quantity (..)
  , StateUpdate (..)
  , StepEntry
  , StepIndex (..)
  )
import AtomicSwap.Types
  ( AdaptedSignature (..)
  , AdapterPoint (..)
  , AdapterSecret (..)
  , Ed25519PrivateKey (..)
  , NIZKProof (..)
  , PrivateKey (..)
  , PublicKey (..)
  , Transaction (..)
  )
import Data.ByteString qualified as BS

--------------------------------------------------------------------------------
-- Initial Page Template -------------------------------------------------------

-- | Main page with 3-column layout (renders current state)
mainPage :: SimulatorState -> Html ()
mainPage simState =
  let aliceState = getPartyState Alice simState
      bobState = getPartyState Bob simState
   in doctypehtml_ do
        head_ do
          meta_ [charset_ "utf-8"]
          meta_
            [ name_ "viewport"
            , content_ "width=device-width, initial-scale=1"
            ]
          title_ "Atomic Swap Simulator"
          -- Iosevka font from jsDelivr CDN
          link_
            [ rel_ "stylesheet"
            , href_ "https://cdn.jsdelivr.net/npm/@fontsource/iosevka@5.0.17/index.css"
            ]
          -- HTMX
          script_ [src_ "https://unpkg.com/htmx.org@1.9.10"] ("" :: Text)
          -- CSS stylesheet
          link_ [rel_ "stylesheet", href_ "/static/style.css"]

        body_ do
          div_ [class_ "container"] do
            header_ [class_ "header"] do
              h1_ "Atomic Swap Simulator"
              p_
                "Step-by-step simulator of atomic swap protocol based on adapter signatures and non-interactive ZK proofs"

            div_ [class_ "main-content"] do
              -- Left column: Alice's state
              div_ [class_ "column alice-column"] do
                h2_ "Alice"

                -- Alice's available actions
                div_ [id_ "alice-actions", class_ "actions-section"] do
                  h3_ "Actions"
                  renderAliceActionsFields simState

                div_ [id_ "alice-state", class_ "state-section"] do
                  h3_ "State"
                  renderAliceStateFields aliceState

              -- Center column: Event timeline
              div_ [class_ "column timeline-column"] do
                div_
                  [ style_
                      "display: flex; justify-content: space-between; align-items: center; margin-bottom: 20px; padding-bottom: 10px; border-bottom: 2px solid #e5e7eb;"
                  ]
                  do
                    h2_ [style_ "margin: 0; border: none; padding: 0;"] "Timeline"
                    button_
                      [ style_
                          "background: transparent; border: 1px solid #d1d5db; color: #9ca3af; padding: 4px 12px; border-radius: 4px; font-size: 12px; cursor: pointer;"
                      , hxPost_ "/reset"
                      , hxTarget_ "body"
                      , hxSwap_ "outerHTML"
                      ]
                      "Reset"
                div_ [id_ "timeline", class_ "timeline"] do
                  renderTimeline (ssSwapAmounts simState) (ssGlobalState simState)
                  renderNextActionHint simState

              -- Right column: Bob's state
              div_ [class_ "column bob-column"] do
                h2_ "Bob"

                -- Bob's available actions
                div_ [id_ "bob-actions", class_ "actions-section"] do
                  h3_ "Actions"
                  renderBobActionsFields bobState

                div_ [id_ "bob-state", class_ "state-section"] do
                  h3_ "State"
                  renderBobStateFields bobState

--------------------------------------------------------------------------------
-- Timeline Rendering ----------------------------------------------------------

-- | Render the complete execution timeline from history
renderTimeline :: (Quantity 'Apple, Quantity 'Banana) -> GlobalState -> Html ()
renderTimeline (Quantity apples, Quantity bananas) globalState = do
  -- Always render Step 0 first
  div_ [class_ "timeline-item"] do
    span_ [class_ "step-number"] "Step 0"
    span_ [class_ "step-description"] $
      toHtml
        ( "Alice and Bob agree to exchange "
            <> show apples
            <> " 🍎 for "
            <> show bananas
            <> " 🍌"
            :: Text
        )
  -- Then render all other steps
  mapM_ renderTimelineEntry globalState

-- | Render hint for next recommended action
renderNextActionHint :: SimulatorState -> Html ()
renderNextActionHint simState =
  case recommendedAction simState of
    Nothing ->
      div_
        [id_ "next-action-hint", class_ "next-action-hint complete"]
        "✓ Protocol complete!"
    Just action ->
      let metadata = actionMetadata action
          participantName = case amParticipant metadata of
            Alice -> "Alice" :: Text
            Bob -> "Bob"
       in div_ [id_ "next-action-hint", class_ "next-action-hint"] do
            span_ [class_ "badge"] "Recommended"
            toHtml $
              "Next step: "
                <> participantName
                <> " should "
                <> amButtonText metadata

-- | Render a single timeline entry
renderTimelineEntry :: StepEntry -> Html ()
renderTimelineEntry (StepIndex idx, participant, _inputs, updates) =
  let eventClass = case participant of
        Alice -> "timeline-item alice-event" :: Text
        Bob -> "timeline-item bob-event" :: Text
      description = describeUpdates participant updates
   in div_ [class_ eventClass] do
        span_ [class_ "step-number"] $ "Step " <> show (idx + 1)
        span_ [class_ "step-description"] $ toHtml description

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
      "Alice generated NIZK proof for commitment"
    [SetOtherPartyNIZKProof Bob _, SetSentNIZKProof Alice _] ->
      "Alice shared NIZK proof with Bob"
    [SetNIZKProofVerified Bob _] ->
      "Bob verified Alice's NIZK proof successfully"
    [SetTransaction Alice _] ->
      "Alice prepared transaction (apples → Bob)"
    [SetPreSignature Alice _] ->
      "Alice created adapter pre-signature"
    [ SetSentPreSignature Alice _
      , SetOtherPartyTransaction Bob _
      , SetOtherPartyPreSignature Bob _
      ] ->
        "Alice published transaction and pre-signature to Bob"
    _ -> case participant of
      Alice -> "Alice performed action"
      Bob -> "Bob performed action"

--------------------------------------------------------------------------------
-- HTMX Helpers ----------------------------------------------------------------

-- | HTMX post attribute
hxPost_ :: Text -> Attributes
hxPost_ url = term "hx-post" url

-- | HTMX target attribute
hxTarget_ :: Text -> Attributes
hxTarget_ target = term "hx-target" target

-- | HTMX swap attribute
hxSwap_ :: Text -> Attributes
hxSwap_ mode = term "hx-swap" mode

-- | HTMX out-of-band swap attribute
hxSwapOob_ :: Text -> Attributes
hxSwapOob_ value = term "hx-swap-oob" value

--------------------------------------------------------------------------------
-- State-Diffing Renderers -----------------------------------------------------

{- | Render state updates for changed parties (state-diffing approach)

Takes list of changed participants and renders their state/action panels
via HTMX out-of-band swaps. Used after detecting which parties changed.
-}
renderStateUpdates :: [Participant] -> SimulatorState -> Html ()
renderStateUpdates changedParties simState =
  let aliceState = getPartyState Alice simState
      bobState = getPartyState Bob simState
   in forM_ changedParties \case
        Alice -> do
          renderAliceStateUpdate aliceState
          renderAliceActionsUpdate simState
        Bob -> do
          renderBobStateUpdate bobState
          renderBobActionsUpdate bobState

{- | Render new timeline entry from the latest step in GlobalState

Reads the last entry from GlobalState to create timeline event.
Also updates hint by deleting old and appending new.
-}
renderNewTimelineEntry :: SimulatorState -> Html ()
renderNewTimelineEntry simState = do
  for_ (viaNonEmpty last (ssGlobalState simState)) renderTimelineEntry
  -- Delete old hint, then append new one
  div_ [hxSwapOob_ "delete:#next-action-hint"] mempty
  renderNextActionHint simState

--------------------------------------------------------------------------------
-- State Rendering -------------------------------------------------------------

-- | Render Alice's state fields (no OOB wrapper)
renderAliceStateFields :: PartyState -> Html ()
renderAliceStateFields partyState = do
  -- Private Key (sk0, sk1)
  case SM.maybe Nothing Just (psPrivateKey partyState) of
    Just (Ed25519PrivateKey (PrivateKey sk0) sk1) -> do
      div_ [class_ "state-item state-set"] do
        span_ [class_ "state-label"] "Private Key (sk0)"
        span_ [class_ "state-value", title_ (formatHex sk0)] $
          toHtml (formatHex sk0)
      div_ [class_ "state-item state-set"] do
        span_ [class_ "state-label"] "Private Key (sk1)"
        span_ [class_ "state-value", title_ (formatHex sk1)] $
          toHtml (formatHex sk1)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Private Key (sk0)"
        span_ [class_ "state-value"] "[not set]"
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Private Key (sk1)"
        span_ [class_ "state-value"] "[not set]"
  -- Public Key
  case SM.maybe Nothing Just (psPublicKey partyState) of
    Just (PublicKey pkBytes) -> do
      div_ [class_ "state-item state-set"] do
        span_ [class_ "state-label"] "Public Key"
        span_ [class_ "state-value", title_ (formatHex pkBytes)] $
          toHtml (formatHex pkBytes)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Public Key"
        span_ [class_ "state-value"] "[not set]"
  -- Bob's Public Key (received from Bob)
  case SM.maybe Nothing Just (psOtherPartyPublicKey partyState) of
    Just (PublicKey pkBytes) -> do
      div_ [class_ "state-item state-set"] do
        span_ [class_ "state-label"] "Bob's Public Key"
        span_ [class_ "state-value", title_ (formatHex pkBytes)] $
          toHtml (formatHex pkBytes)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Bob's Public Key"
        span_ [class_ "state-value"] "[not set]"
  -- Adapter Secret
  case SM.maybe Nothing Just (psAdapterSecret partyState) of
    Just (AdapterSecret secretBytes) -> do
      div_ [class_ "state-item state-set"] do
        span_ [class_ "state-label"] "Adapter Secret (y)"
        span_ [class_ "state-value", title_ (formatHex secretBytes)] $
          toHtml (formatHex secretBytes)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Adapter Secret (y)"
        span_ [class_ "state-value"] "[not set]"
  -- Commitment
  case SM.maybe Nothing Just (psAdapterCommitment partyState) of
    Just (AdapterPoint commitBytes) -> do
      div_ [class_ "state-item state-set"] do
        span_ [class_ "state-label"] "Seal (Y)"
        span_ [class_ "state-value", title_ (formatHex commitBytes)] $
          toHtml (formatHex commitBytes)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Seal (Y)"
        span_ [class_ "state-value"] "[not set]"
  -- NIZK Proof
  case SM.maybe Nothing Just (psNIZKProof partyState) of
    Just (NIZKProof proofBytes) -> do
      div_ [class_ "state-item state-set"] do
        span_ [class_ "state-label"] "NIZK Proof"
        span_ [class_ "state-value", title_ (formatHex proofBytes)] $
          toHtml (formatHex proofBytes)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "NIZK Proof"
        span_ [class_ "state-value"] "[not set]"
  -- Transaction
  case SM.maybe Nothing Just (psTransaction partyState) of
    Just tx -> do
      div_ [class_ "state-item state-set"] do
        span_ [class_ "state-label"] "Transaction"
        span_ [class_ "state-value"] $
          toHtml
            ( show (length (txInputs tx))
                <> " inputs, "
                <> show (length (txOutputs tx))
                <> " outputs"
                :: Text
            )
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Transaction"
        span_ [class_ "state-value"] "[not set]"
  -- Pre-Signature
  case SM.maybe Nothing Just (psPreSignature partyState) of
    Just (AdaptedSignature rSign sigTilde) -> do
      div_ [class_ "state-item state-set"] do
        span_ [class_ "state-label"] "Pre-Signature (σ̃)"
        span_
          [class_ "state-value", title_ (formatHex rSign <> " || " <> formatHex sigTilde)]
          $ toHtml
          $ formatHex (BS.take 16 rSign) <> "..." <> formatHex (BS.take 8 sigTilde)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Pre-Signature (σ̃)"
        span_ [class_ "state-value"] "[not set]"

-- | Render Alice's complete state (with OOB wrapper)
renderAliceStateUpdate :: PartyState -> Html ()
renderAliceStateUpdate partyState = do
  div_ [hxSwapOob_ "innerHTML:#alice-state"] do
    h3_ "State"
    renderAliceStateFields partyState

-- | Render Bob's state fields (no OOB wrapper)
renderBobStateFields :: PartyState -> Html ()
renderBobStateFields partyState = do
  -- Private Key (sk0, sk1)
  case SM.maybe Nothing Just (psPrivateKey partyState) of
    Just (Ed25519PrivateKey (PrivateKey sk0) sk1) -> do
      div_ [class_ "state-item state-set"] do
        span_ [class_ "state-label"] "Private Key (sk0)"
        span_ [class_ "state-value", title_ (formatHex sk0)] $
          toHtml $
            formatHex sk0
      div_ [class_ "state-item state-set"] do
        span_ [class_ "state-label"] "Private Key (sk1)"
        span_ [class_ "state-value", title_ (formatHex sk1)] $
          toHtml $
            formatHex sk1
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Private Key (sk0)"
        span_ [class_ "state-value"] "[not set]"
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Private Key (sk1)"
        span_ [class_ "state-value"] "[not set]"
  -- Public Key
  case SM.maybe Nothing Just (psPublicKey partyState) of
    Just (PublicKey pkBytes) -> do
      div_ [class_ "state-item state-set"] do
        span_ [class_ "state-label"] "Public Key"
        span_ [class_ "state-value", title_ (formatHex pkBytes)] $
          toHtml $
            formatHex pkBytes
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Public Key"
        span_ [class_ "state-value"] "[not set]"
  -- Alice's Public Key (received from Alice)
  case SM.maybe Nothing Just (psOtherPartyPublicKey partyState) of
    Just (PublicKey pkBytes) -> do
      div_ [class_ "state-item state-set"] do
        span_ [class_ "state-label"] "Alice's Public Key"
        span_ [class_ "state-value", title_ (formatHex pkBytes)] $
          toHtml $
            formatHex pkBytes
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Alice's Public Key"
        span_ [class_ "state-value"] "[not set]"
  -- Alice's Commitment (received from Alice)
  case SM.maybe Nothing Just (psOtherPartyCommitment partyState) of
    Just (AdapterPoint commitBytes) -> do
      div_ [class_ "state-item state-set"] do
        span_ [class_ "state-label"] "Alice's Seal (Y)"
        span_ [class_ "state-value", title_ (formatHex commitBytes)] $
          toHtml $
            formatHex commitBytes
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Alice's Seal (Y)"
        span_ [class_ "state-value"] "[not set]"
  -- Alice's NIZK Proof (received from Alice)
  case SM.maybe Nothing Just (psOtherPartyNIZKProof partyState) of
    Just (NIZKProof proofBytes) -> do
      div_ [class_ "state-item state-set"] do
        span_ [class_ "state-label"] "Alice's NIZK Proof"
        span_ [class_ "state-value", title_ (formatHex proofBytes)] $
          toHtml $
            formatHex proofBytes
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Alice's NIZK Proof"
        span_ [class_ "state-value"] "[not set]"
  -- NIZK Proof Verification Status
  if psNIZKProofVerified partyState
    then div_ [class_ "state-item state-set"] do
      span_ [class_ "state-label"] "NIZK Proof Status"
      span_ [class_ "state-value"] "✓ Verified"
    else div_ [class_ "state-item state-unset"] do
      span_ [class_ "state-label"] "NIZK Proof Status"
      span_ [class_ "state-value"] "[not verified]"
  -- Alice's Transaction (received from Alice)
  case SM.maybe Nothing Just (psOtherPartyTransaction partyState) of
    Just tx -> do
      div_ [class_ "state-item state-set"] do
        span_ [class_ "state-label"] "Alice's Transaction"
        span_ [class_ "state-value"] $
          toHtml
            ( show (length (txInputs tx))
                <> " inputs, "
                <> show (length (txOutputs tx))
                <> " outputs"
                :: Text
            )
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Alice's Transaction"
        span_ [class_ "state-value"] "[not set]"
  -- Alice's Pre-Signature (received from Alice)
  case SM.maybe Nothing Just (psOtherPartyPreSignature partyState) of
    Just (AdaptedSignature rSign sigTilde) -> do
      div_ [class_ "state-item state-set"] do
        span_ [class_ "state-label"] "Alice's Pre-Sig (σ̃ᴬ)"
        span_
          [class_ "state-value", title_ (formatHex rSign <> " || " <> formatHex sigTilde)]
          $ toHtml
          $ formatHex (BS.take 16 rSign) <> "..." <> formatHex (BS.take 8 sigTilde)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Alice's Pre-Sig (σ̃ᴬ)"
        span_ [class_ "state-value"] "[not set]"
  -- Extracted Secret (Bob only)
  div_ [class_ "state-item state-unset"] do
    span_ [class_ "state-label"] "Extracted Secret"
    span_ [class_ "state-value"] "[not set]"

-- | Render Bob's complete state (with OOB wrapper)
renderBobStateUpdate :: PartyState -> Html ()
renderBobStateUpdate partyState = do
  div_ [hxSwapOob_ "innerHTML:#bob-state"] do
    h3_ "State"
    renderBobStateFields partyState

--------------------------------------------------------------------------------
-- Action Rendering ------------------------------------------------------------

-- | Render Alice's action buttons (no OOB wrapper)
renderAliceActionsFields :: SimulatorState -> Html ()
renderAliceActionsFields simState =
  let aliceActions = filter isAliceAction (availableActions simState)
      hasActions = not (null aliceActions)
   in if hasActions
        then
          div_ [class_ "button-group"] $
            mapM_ renderActionButton aliceActions
        else div_ [class_ "no-actions"] "Waiting for Bob..."
  where
    isAliceAction :: Action -> Bool
    isAliceAction action =
      let metadata = actionMetadata action
       in amParticipant metadata == Alice

    renderActionButton :: Action -> Html ()
    renderActionButton action =
      let metadata = actionMetadata action
       in button_
            [ class_ "party-button alice-button"
            , hxPost_ (amEndpoint metadata)
            , hxTarget_ "#timeline"
            , hxSwap_ "beforeend"
            ]
            (toHtml $ amButtonText metadata)

-- | Render Alice's actions with OOB wrapper
renderAliceActionsUpdate :: SimulatorState -> Html ()
renderAliceActionsUpdate simState =
  div_ [hxSwapOob_ "innerHTML:#alice-actions"] do
    h3_ "Actions"
    renderAliceActionsFields simState

-- | Render Bob's action buttons (no OOB wrapper)
renderBobActionsFields :: PartyState -> Html ()
renderBobActionsFields partyState = do
  let hasPublicKey = SM.isJust (psPublicKey partyState)
      sentPublicKey = psSentPublicKey partyState
      hasCommitment = SM.isJust (psOtherPartyCommitment partyState)
      hasProof = SM.isJust (psOtherPartyNIZKProof partyState)
      proofVerified = psNIZKProofVerified partyState
      canVerifyProof = hasCommitment && hasProof && not proofVerified
      hasAnyActions = not hasPublicKey || (hasPublicKey && not sentPublicKey) || canVerifyProof

  if not hasAnyActions
    then div_ [class_ "no-actions"] "Waiting for Alice..."
    else div_ [class_ "button-group"] do
      -- Show keypair button ONLY if not generated
      unless hasPublicKey $
        button_
          [ class_ "party-button bob-button"
          , hxPost_ "/step/bob-keygen"
          , hxTarget_ "#timeline"
          , hxSwap_ "beforeend"
          ]
          "Generate Keys"
      -- Show send public key button only if has key but hasn't sent yet
      when (hasPublicKey && not sentPublicKey) $
        button_
          [ class_ "party-button bob-button"
          , hxPost_ "/step/bob-send-public-key"
          , hxTarget_ "#timeline"
          , hxSwap_ "beforeend"
          ]
          "Publish Pubkey"
      -- Show verify NIZK proof button if has both commitment and proof but not verified
      when canVerifyProof $
        button_
          [ class_ "party-button bob-button"
          , hxPost_ "/step/bob-verify-nizk-proof"
          , hxTarget_ "#timeline"
          , hxSwap_ "beforeend"
          ]
          "Verify Proof"

-- | Render Bob's actions with OOB wrapper
renderBobActionsUpdate :: PartyState -> Html ()
renderBobActionsUpdate partyState = do
  div_ [hxSwapOob_ "innerHTML:#bob-actions"] do
    h3_ "Actions"
    renderBobActionsFields partyState

--------------------------------------------------------------------------------
-- Helper Functions ------------------------------------------------------------

-- | Format bytes as hex string
formatHex :: ByteString -> Text
formatHex = TE.decodeUtf8 . Base16.encode
