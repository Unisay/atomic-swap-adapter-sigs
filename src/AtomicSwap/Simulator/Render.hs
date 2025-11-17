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
  , Signature (..)
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
                  renderNextActionHint simState
                  div_ [id_ "timeline-entries"] do
                    renderTimeline (ssSwapAmounts simState) (ssGlobalState simState)

              -- Right column: Bob's state
              div_ [class_ "column bob-column"] do
                h2_ "Bob"

                -- Bob's available actions
                div_ [id_ "bob-actions", class_ "actions-section"] do
                  h3_ "Actions"
                  renderBobActionsFields simState

                div_ [id_ "bob-state", class_ "state-section"] do
                  h3_ "State"
                  renderBobStateFields bobState

--------------------------------------------------------------------------------
-- Timeline Rendering ----------------------------------------------------------

-- | Render the complete execution timeline from history
renderTimeline :: (Quantity 'Apple, Quantity 'Banana) -> GlobalState -> Html ()
renderTimeline (Quantity apples, Quantity bananas) globalState = do
  -- Render all steps in REVERSE order (newest first) since we're using afterbegin
  mapM_ renderTimelineEntry (reverse globalState)
  -- Render Step 0 last so it appears at the bottom
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

IMPORTANT: Action buttons are ALWAYS updated for BOTH parties, even if only one
party's state changed, because the recommended action is global and can change
when the other party acts.
-}
renderStateUpdates :: [Participant] -> SimulatorState -> Html ()
renderStateUpdates changedParties simState =
  let aliceState = getPartyState Alice simState
      bobState = getPartyState Bob simState
   in do
        -- Update state panels only for changed parties
        forM_ changedParties \case
          Alice -> renderAliceStateUpdate aliceState
          Bob -> renderBobStateUpdate bobState
        -- ALWAYS update action buttons for BOTH parties (recommendation can change)
        renderAliceActionsUpdate simState
        renderBobActionsUpdate simState

{- | Render new timeline entry from the latest step in GlobalState

Reads the last entry from GlobalState to create timeline event.
Also updates hint by deleting old and appending new.
-}
renderNewTimelineEntry :: SimulatorState -> Html ()
renderNewTimelineEntry simState = do
  for_ (viaNonEmpty last (ssGlobalState simState)) renderTimelineEntry
  -- Update hint with OOB swap (no need to delete first, OOB replaces it)
  renderNextActionHintWithOob True simState

--------------------------------------------------------------------------------
-- State Rendering -------------------------------------------------------------

-- | Render Alice's state fields (no OOB wrapper)
renderAliceStateFields :: PartyState -> Html ()
renderAliceStateFields partyState = do
  let stateClass isFresh =
        if isFresh
          then "state-item state-set recently-updated"
          else "state-item state-set"
  -- Private Key (sk0, sk1)
  case SM.maybe Nothing Just (psPrivateKey partyState) of
    Just (Ed25519PrivateKey (PrivateKey sk0) sk1) -> do
      div_ [class_ (stateClass (psPrivateKeyFresh partyState))] do
        span_ [class_ "state-label"] "Private Key (sk0)"
        span_ [class_ "state-value", title_ (formatHex sk0)] $
          toHtml (formatHex sk0)
      div_ [class_ (stateClass (psPrivateKeyFresh partyState))] do
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
      div_ [class_ (stateClass (psPublicKeyFresh partyState))] do
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
      div_ [class_ (stateClass (psOtherPartyPublicKeyFresh partyState))] do
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
      div_ [class_ (stateClass (psAdapterSecretFresh partyState))] do
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
      div_ [class_ (stateClass (psAdapterCommitmentFresh partyState))] do
        span_ [class_ "state-label"] "Seal (Y)"
        span_ [class_ "state-value", title_ (formatHex commitBytes)] $
          toHtml (formatHex commitBytes)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Seal (Y)"
        span_ [class_ "state-value"] "[not set]"
  -- Seal Proof
  case SM.maybe Nothing Just (psNIZKProof partyState) of
    Just (NIZKProof proofBytes) -> do
      div_ [class_ (stateClass (psNIZKProofFresh partyState))] do
        span_ [class_ "state-label"] "Seal Proof"
        span_ [class_ "state-value", title_ (formatHex proofBytes)] $
          toHtml (formatHex proofBytes)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Seal Proof"
        span_ [class_ "state-value"] "[not set]"
  -- Transaction
  case SM.maybe Nothing Just (psTransaction partyState) of
    Just tx -> do
      div_ [class_ (stateClass (psTransactionFresh partyState))] do
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
      div_ [class_ (stateClass (psPreSignatureFresh partyState))] do
        span_ [class_ "state-label"] "Pre-Signature (σ̃)"
        span_
          [class_ "state-value", title_ (formatHex rSign <> " || " <> formatHex sigTilde)]
          $ toHtml
          $ formatHex (BS.take 16 rSign) <> "..." <> formatHex (BS.take 8 sigTilde)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Pre-Signature (σ̃)"
        span_ [class_ "state-value"] "[not set]"
  -- Alice's Complete Signature
  case SM.maybe Nothing Just (psCompleteSignature partyState) of
    Just (Signature rSign sig) -> do
      div_ [class_ (stateClass (psCompleteSignatureFresh partyState))] do
        span_ [class_ "state-label"] "Complete Sig (σᴬ)"
        span_
          [class_ "state-value", title_ (formatHex rSign <> " || " <> formatHex sig)]
          $ toHtml
          $ formatHex (BS.take 16 rSign) <> "..." <> formatHex (BS.take 8 sig)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Complete Sig (σᴬ)"
        span_ [class_ "state-value"] "[not set]"
  -- Bob's Transaction (received from Bob)
  case SM.maybe Nothing Just (psOtherPartyTransaction partyState) of
    Just tx -> do
      div_ [class_ (stateClass (psOtherPartyTransactionFresh partyState))] do
        span_ [class_ "state-label"] "Bob's Transaction"
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
        span_ [class_ "state-label"] "Bob's Transaction"
        span_ [class_ "state-value"] "[not set]"
  -- Bob's Pre-Signature (received from Bob)
  case SM.maybe Nothing Just (psOtherPartyPreSignature partyState) of
    Just (AdaptedSignature rSign sigTilde) -> do
      div_ [class_ (stateClass (psOtherPartyPreSignatureFresh partyState))] do
        span_ [class_ "state-label"] "Bob's Pre-Sig (σ̃ᴮ)"
        span_
          [class_ "state-value", title_ (formatHex rSign <> " || " <> formatHex sigTilde)]
          $ toHtml
          $ formatHex (BS.take 16 rSign) <> "..." <> formatHex (BS.take 8 sigTilde)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Bob's Pre-Sig (σ̃ᴮ)"
        span_ [class_ "state-value"] "[not set]"
  -- Pre-Signature Verification Status
  if psPreSignatureVerified partyState
    then div_ [class_ "state-item state-set"] do
      span_ [class_ "state-label"] "Pre-Sig Status"
      span_ [class_ "state-value"] "✓ Verified"
    else div_ [class_ "state-item state-unset"] do
      span_ [class_ "state-label"] "Pre-Sig Status"
      span_ [class_ "state-value"] "[not verified]"

-- | Render Alice's complete state (with OOB wrapper)
renderAliceStateUpdate :: PartyState -> Html ()
renderAliceStateUpdate partyState = do
  div_ [hxSwapOob_ "innerHTML:#alice-state"] do
    h3_ "State"
    renderAliceStateFields partyState

-- | Render Bob's state fields (no OOB wrapper)
renderBobStateFields :: PartyState -> Html ()
renderBobStateFields partyState = do
  let stateClass isFresh =
        if isFresh
          then "state-item state-set recently-updated"
          else "state-item state-set"
  -- Private Key (sk0, sk1)
  case SM.maybe Nothing Just (psPrivateKey partyState) of
    Just (Ed25519PrivateKey (PrivateKey sk0) sk1) -> do
      div_ [class_ (stateClass (psPrivateKeyFresh partyState))] do
        span_ [class_ "state-label"] "Private Key (sk0)"
        span_ [class_ "state-value", title_ (formatHex sk0)] $
          toHtml $
            formatHex sk0
      div_ [class_ (stateClass (psPrivateKeyFresh partyState))] do
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
      div_ [class_ (stateClass (psPublicKeyFresh partyState))] do
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
  -- Alice's Seal Proof (received from Alice)
  case SM.maybe Nothing Just (psOtherPartyNIZKProof partyState) of
    Just (NIZKProof proofBytes) -> do
      div_ [class_ "state-item state-set"] do
        span_ [class_ "state-label"] "Alice's Seal Proof"
        span_ [class_ "state-value", title_ (formatHex proofBytes)] $
          toHtml $
            formatHex proofBytes
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Alice's Seal Proof"
        span_ [class_ "state-value"] "[not set]"
  -- Seal Proof Verification Status
  if psNIZKProofVerified partyState
    then div_ [class_ "state-item state-set"] do
      span_ [class_ "state-label"] "Seal Proof Status"
      span_ [class_ "state-value"] "✓ Verified"
    else div_ [class_ "state-item state-unset"] do
      span_ [class_ "state-label"] "Seal Proof Status"
      span_ [class_ "state-value"] "[not verified]"
  -- Bob's Own Transaction
  case SM.maybe Nothing Just (psTransaction partyState) of
    Just tx -> do
      div_ [class_ (stateClass (psTransactionFresh partyState))] do
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
  -- Bob's Own Pre-Signature
  case SM.maybe Nothing Just (psPreSignature partyState) of
    Just (AdaptedSignature rSign sigTilde) -> do
      div_ [class_ (stateClass (psPreSignatureFresh partyState))] do
        span_ [class_ "state-label"] "Pre-Signature (σ̃ᴮ)"
        span_
          [class_ "state-value", title_ (formatHex rSign <> " || " <> formatHex sigTilde)]
          $ toHtml
          $ formatHex (BS.take 16 rSign) <> "..." <> formatHex (BS.take 8 sigTilde)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Pre-Signature (σ̃ᴮ)"
        span_ [class_ "state-value"] "[not set]"
  -- Alice's Transaction (received from Alice)
  case SM.maybe Nothing Just (psOtherPartyTransaction partyState) of
    Just tx -> do
      div_ [class_ (stateClass (psOtherPartyTransactionFresh partyState))] do
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
      div_ [class_ (stateClass (psOtherPartyPreSignatureFresh partyState))] do
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
  case SM.maybe Nothing Just (psExtractedSecret partyState) of
    Just (AdapterSecret secretBytes) -> do
      div_ [class_ (stateClass (psExtractedSecretFresh partyState))] do
        span_ [class_ "state-label"] "Extracted Secret (y)"
        span_ [class_ "state-value", title_ (formatHex secretBytes)] $
          toHtml (formatHex secretBytes)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Extracted Secret (y)"
        span_ [class_ "state-value"] "[not set]"
  -- Bob's Complete Signature
  case SM.maybe Nothing Just (psCompleteSignature partyState) of
    Just (Signature rSign sig) -> do
      div_ [class_ (stateClass (psCompleteSignatureFresh partyState))] do
        span_ [class_ "state-label"] "Complete Sig (σᴮ)"
        span_
          [class_ "state-value", title_ (formatHex rSign <> " || " <> formatHex sig)]
          $ toHtml
          $ formatHex (BS.take 16 rSign) <> "..." <> formatHex (BS.take 8 sig)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Complete Sig (σᴮ)"
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
      recommended = recommendedAction simState
   in if hasActions
        then
          div_ [class_ "button-group"] $
            mapM_ (renderActionButton recommended) aliceActions
        else div_ [class_ "no-actions"] "Waiting for Bob..."
  where
    isAliceAction :: Action -> Bool
    isAliceAction action =
      let metadata = actionMetadata action
       in amParticipant metadata == Alice

    renderActionButton :: Maybe Action -> Action -> Html ()
    renderActionButton recommended action =
      let metadata = actionMetadata action
          isRecommended = Just action == recommended
          buttonClass =
            if isRecommended
              then "party-button alice-button"
              else "party-button alice-button non-recommended"
       in button_
            [ class_ buttonClass
            , hxPost_ (amEndpoint metadata)
            , hxTarget_ "#timeline-entries"
            , hxSwap_ "afterbegin"
            ]
            (toHtml $ amButtonText metadata)

-- | Render Alice's actions with OOB wrapper
renderAliceActionsUpdate :: SimulatorState -> Html ()
renderAliceActionsUpdate simState =
  div_ [hxSwapOob_ "innerHTML:#alice-actions"] do
    h3_ "Actions"
    renderAliceActionsFields simState

-- | Render Bob's action buttons (no OOB wrapper)
renderBobActionsFields :: SimulatorState -> Html ()
renderBobActionsFields simState =
  let bobActions = filter isBobAction (availableActions simState)
      hasActions = not (null bobActions)
      recommended = recommendedAction simState
   in if hasActions
        then
          div_ [class_ "button-group"] $
            mapM_ (renderActionButton recommended) bobActions
        else div_ [class_ "no-actions"] "Waiting for Alice..."
  where
    isBobAction :: Action -> Bool
    isBobAction action =
      let metadata = actionMetadata action
       in amParticipant metadata == Bob

    renderActionButton :: Maybe Action -> Action -> Html ()
    renderActionButton recommended action =
      let metadata = actionMetadata action
          isRecommended = Just action == recommended
          buttonClass =
            if isRecommended
              then "party-button bob-button"
              else "party-button bob-button non-recommended"
       in button_
            [ class_ buttonClass
            , hxPost_ (amEndpoint metadata)
            , hxTarget_ "#timeline-entries"
            , hxSwap_ "afterbegin"
            ]
            (toHtml $ amButtonText metadata)

-- | Render Bob's actions with OOB wrapper
renderBobActionsUpdate :: SimulatorState -> Html ()
renderBobActionsUpdate simState = do
  div_ [hxSwapOob_ "innerHTML:#bob-actions"] do
    h3_ "Actions"
    renderBobActionsFields simState

--------------------------------------------------------------------------------
-- Helper Functions ------------------------------------------------------------

-- | Format bytes as hex string
formatHex :: ByteString -> Text
formatHex = TE.decodeUtf8 . Base16.encode
