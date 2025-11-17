{- |
Module: AtomicSwap.Prelude
Description: Custom prelude for atomic swap project

This module exports project-specific utilities, types, and io-classes
concurrency primitives for io-sim compatibility.

NOTE: Modules using this prelude still need to import Relude separately.
This module only provides io-classes STM primitives and project types.
-}
module AtomicSwap.Prelude
  ( -- * Core Types
    module AtomicSwap.Types

    -- * Concurrency Primitives (io-classes, io-sim compatible)
  , module Control.Monad.Class.MonadSTM
  , module Control.Concurrent.Class.MonadSTM.TVar
  , module Control.Concurrent.Class.MonadSTM.TQueue

    -- * Logging Utilities
  , logInfo
  , logPhase

    -- * Useful Type Synonyms
  , EitherText
  ) where

import AtomicSwap.Types
  ( AdaptedSignature (..)
  , AdapterPoint (..)
  , AdapterSecret (..)
  , Ed25519PrivateKey (..)
  , Message (..)
  , NIZKProof (..)
  , Output (..)
  , Party (..)
  , PartyName
  , PrivateKey (..)
  , PublicKey (..)
  , Signature (..)
  , SwapResult (..)
  , SwapState (..)
  , Transaction (..)
  , TxId (..)
  , UTXO (..)
  )

-- Re-export io-classes STM primitives for io-sim compatibility
import Control.Concurrent.Class.MonadSTM.TQueue
import Control.Concurrent.Class.MonadSTM.TVar
import Control.Monad.Class.MonadSTM

--------------------------------------------------------------------------------
-- Logging Utilities -----------------------------------------------------------

-- | Log an informational message with party identification
logInfo :: MonadIO m => PartyName -> Text -> m ()
logInfo party msg = putTextLn ("[" <> party <> "] " <> msg)

-- | Log a protocol phase header
logPhase :: MonadIO m => Text -> m ()
logPhase phase =
  putTextLn ""
    >> putTextLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    >> putTextLn phase
    >> putTextLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    >> putTextLn ""

--------------------------------------------------------------------------------
-- Type Synonyms ---------------------------------------------------------------

-- | Common Either pattern with Text as the error type
type EitherText a = Either Text a
