{-# LANGUAGE StrictData #-}

{- |
Module: AtomicSwap.Simulator.Render.Helpers
Description: Utility functions for HTML rendering

Provides helper functions used across rendering modules including HTMX
attributes, icon selection, and formatting utilities.
-}
module AtomicSwap.Simulator.Render.Helpers
  ( -- * HTML Helpers
    section_
  , stepIcon_
  , stateValueInput_

    -- * Icon Selection
  , getStepIcon

    -- * HTMX Attributes
  , hxPost_
  , hxTarget_
  , hxSwap_
  , hxSwapOob_

    -- * Formatting
  , formatHex
  ) where

import Data.ByteString.Base16 qualified as Base16
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Lucid
  ( Attributes
  , Html
  , class_
  , div_
  , img_
  , span_
  , src_
  , term
  , toHtml
  )
import Prelude

--------------------------------------------------------------------------------
-- HTML Helpers ----------------------------------------------------------------

-- | Section header with horizontal line and overlaid label
section_ :: Text -> Html ()
section_ label =
  div_ [class_ "section-header"] do
    span_ [class_ "section-label"] (toHtml label)

-- | Icon element (renders SVG from static/icons/)
stepIcon_ :: Text -> Html ()
stepIcon_ iconName =
  img_ [class_ "step-icon", src_ ("/static/icons/" <> iconName <> ".svg")]

-- | Render a readonly input field for state values (allows full text selection)
stateValueInput_ :: Text -> Html ()
stateValueInput_ value =
  term
    "input"
    [ class_ "state-value"
    , term "type" "text"
    , term "readonly" ""
    , term "value" value
    ]
    ""

--------------------------------------------------------------------------------
-- Icon Selection --------------------------------------------------------------

-- | Determine icon based on step description keywords
getStepIcon :: Text -> Text
getStepIcon description
  | "generated Ed25519 keypair" `T.isInfixOf` description = "key"
  | "generated adapter secret" `T.isInfixOf` description = "lock"
  | "computed commitment" `T.isInfixOf` description = "lock"
  | "shared" `T.isInfixOf` description = "arrow-right"
  | "generated seal proof" `T.isInfixOf` description = "badge-check"
  | "verified" `T.isInfixOf` description = "check-circle"
  | "prepared transaction" `T.isInfixOf` description = "card-wallet"
  | "created adapter pre-signature" `T.isInfixOf` description = "lock"
  | "published transaction" `T.isInfixOf` description = "card-wallet"
  | "completed signature" `T.isInfixOf` description = "check-circle"
  | "extracted adapter secret" `T.isInfixOf` description = "key"
  | otherwise = "check-circle" -- Default icon

--------------------------------------------------------------------------------
-- HTMX Attributes -------------------------------------------------------------

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
-- Formatting ------------------------------------------------------------------

-- | Format bytes as hex string
formatHex :: ByteString -> Text
formatHex = TE.decodeUtf8 . Base16.encode
