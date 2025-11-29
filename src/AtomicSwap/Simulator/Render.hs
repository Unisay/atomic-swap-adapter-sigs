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
  ( Html
  , body_
  , charset_
  , class_
  , content_
  , div_
  , doctypehtml_
  , h1_
  , h2_
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
  , src_
  , title_
  )
import Prelude

import AtomicSwap.Simulator.Render.Actions
  ( renderAliceActionsFields
  , renderAliceActionsUpdate
  , renderBobActionsFields
  , renderBobActionsUpdate
  )
import AtomicSwap.Simulator.Render.Helpers (section_)
import AtomicSwap.Simulator.Render.State
  ( renderAliceBlockchain
  , renderAliceBlockchainUpdate
  , renderAliceState
  , renderAliceStateUpdate
  , renderBobBlockchain
  , renderBobBlockchainUpdate
  , renderBobState
  , renderBobStateUpdate
  )
import AtomicSwap.Simulator.Render.Timeline
  ( renderNewTimelineEntry
  , renderNextActionHint
  , renderTimeline
  )
import AtomicSwap.Simulator.State
  ( SimulatorState (..)
  , getPartyState
  )
import AtomicSwap.Simulator.Types (Participant (..))

--------------------------------------------------------------------------------
-- Main Page Template ----------------------------------------------------------

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
          -- HTMX
          script_ [src_ "https://unpkg.com/htmx.org@1.9.10"] ("" :: Text)
          -- CSS stylesheet
          link_ [rel_ "stylesheet", href_ "/static/style.css"]

        body_ do
          -- JavaScript for select-all on click for readonly inputs
          script_ $
            unlines
              [ "document.addEventListener('click', function(e) {"
              , "  if (e.target.tagName === 'INPUT' && e.target.hasAttribute('readonly') && e.target.value) {"
              , "    e.target.select();"
              , "  }"
              , "});"
              ]

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
                  section_ "Actions"
                  renderAliceActionsFields simState

                div_ [id_ "alice-state", class_ "state-section"] do
                  section_ "State"
                  uncurry renderAliceState (ssSwapAmounts simState) aliceState

                div_ [id_ "alice-blockchain", class_ "blockchain-section"] do
                  section_ "Blockchain"
                  uncurry renderAliceBlockchain (ssSwapAmounts simState) aliceState

              -- Center column: Event timeline
              div_ [class_ "column timeline-column"] do
                div_ [class_ "timeline-header"] do
                  h2_ "Timeline"
                div_ [id_ "timeline", class_ "timeline"] do
                  renderNextActionHint simState
                  div_ [id_ "timeline-entries"] do
                    renderTimeline (ssSwapAmounts simState) (ssGlobalState simState)

              -- Right column: Bob's state
              div_ [class_ "column bob-column"] do
                h2_ "Bob"

                -- Bob's available actions
                div_ [id_ "bob-actions", class_ "actions-section"] do
                  section_ "Actions"
                  renderBobActionsFields simState

                div_ [id_ "bob-state", class_ "state-section"] do
                  section_ "State"
                  uncurry renderBobState (ssSwapAmounts simState) bobState

                div_ [id_ "bob-blockchain", class_ "blockchain-section"] do
                  section_ "Blockchain"
                  uncurry renderBobBlockchain (ssSwapAmounts simState) bobState

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
      (apples, bananas) = ssSwapAmounts simState
   in do
        -- Update state panels only for changed parties
        forM_ changedParties \case
          Alice -> do
            renderAliceStateUpdate apples bananas aliceState
            renderAliceBlockchainUpdate apples bananas aliceState
          Bob -> do
            renderBobStateUpdate apples bananas bobState
            renderBobBlockchainUpdate apples bananas bobState
        -- ALWAYS update action buttons for BOTH parties (recommendation can change)
        renderAliceActionsUpdate simState
        renderBobActionsUpdate simState
