{-# LANGUAGE StrictData #-}

{- |
Module: AtomicSwap.Simulator.Render.Actions
Description: Action button rendering for both participants

Provides rendering functions for Alice's and Bob's action button panels
including available actions and recommended action highlighting.
-}
module AtomicSwap.Simulator.Render.Actions
  ( -- * Action Field Rendering
    renderAliceActionsFields
  , renderBobActionsFields

    -- * OOB Update Wrappers
  , renderAliceActionsUpdate
  , renderBobActionsUpdate
  ) where

import Lucid
  ( Html
  , button_
  , class_
  , div_
  , toHtml
  )
import Prelude

import AtomicSwap.Simulator.Actions
  ( ActionMetadata (..)
  , actionMetadata
  , availableActions
  , recommendedAction
  )
import AtomicSwap.Simulator.Render.Helpers
  ( hxPost_
  , hxSwapOob_
  , hxSwap_
  , hxTarget_
  , section_
  )
import AtomicSwap.Simulator.State (SimulatorState)
import AtomicSwap.Simulator.Types (Action, Participant (..))

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
    section_ "Actions"
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
    section_ "Actions"
    renderBobActionsFields simState

--------------------------------------------------------------------------------
-- Helper Functions ------------------------------------------------------------
