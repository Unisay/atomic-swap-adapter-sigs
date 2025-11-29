{- |
Module: AtomicSwap.Logging
Description: Structured logging utilities for atomic swap protocol

This module provides comprehensive logging functions for tracking protocol
execution, cryptographic operations, and transaction flow. All logging is
verbose to aid in understanding the atomic swap process.
-}
module AtomicSwap.Logging
  ( -- * Phase Logging
    logPhase
  , logSubPhase

    -- * Party Logging
  , logInfo
  , logAction
  , logSecret
  , logError

    -- * Transaction Logging
  , logTransaction
  , logSignature
  , logPublicKey

    -- * Utility Functions
  , logSeparator
  , formatHex
  ) where

import AtomicSwap.Types (Participant)
import Data.ByteString.Base16 qualified as Base16
import Data.Text.Encoding qualified as TE

--------------------------------------------------------------------------------
-- Phase Logging ---------------------------------------------------------------

{- |
Log a major protocol phase with prominent formatting.

Args:
  - phase: The name of the protocol phase
-}
logPhase :: Text -> IO ()
logPhase phase = do
  putTextLn ""
  putTextLn
    "================================================================================"
  putTextLn $ "  " <> phase
  putTextLn
    "================================================================================"
  putTextLn ""

{- |
Log a sub-phase within a major protocol phase.

Args:
  - subPhase: The name of the sub-phase
-}
logSubPhase :: Text -> IO ()
logSubPhase subPhase = do
  putTextLn ""
  let padding = 72 - length (toString subPhase)
      dashes = toText (replicate padding '-')
  putTextLn $ "--- " <> subPhase <> " " <> dashes
  putTextLn ""

--------------------------------------------------------------------------------
-- Party Logging ---------------------------------------------------------------

{- |
Log an informational message from a specific party.

Args:
  - party: The party participant (Alice or Bob)
  - message: The informational message
-}
logInfo :: Participant -> Text -> IO ()
logInfo party message = do
  putTextLn $ "[" <> show party <> "] " <> message

{- |
Log an action being performed by a party.

Args:
  - party: The party participant
  - action: Description of the action
-}
logAction :: Participant -> Text -> IO ()
logAction party action = do
  putTextLn $ "[" <> show party <> "] ➜ " <> action

{- |
Log a secret value in hexadecimal format (WARNING: use only for debugging).

Args:
  - party: The party participant
  - label: Description of the secret
  - secret: The secret bytes
-}
logSecret :: Participant -> Text -> ByteString -> IO ()
logSecret party label secret = do
  putTextLn $ "[" <> show party <> "] 🔒 " <> label <> ": " <> formatHex secret

{- |
Log an error message from a party.

Args:
  - party: The party participant
  - errorMsg: The error message
-}
logError :: Participant -> Text -> IO ()
logError party errorMsg = do
  putTextLn $ "[" <> show party <> "] ❌ ERROR: " <> errorMsg

--------------------------------------------------------------------------------
-- Transaction Logging ---------------------------------------------------------

{- |
Log transaction details in human-readable format.

Args:
  - party: The party participant logging the transaction
  - label: Description of the transaction
  - txId: Transaction ID
-}
logTransaction :: Participant -> Text -> ByteString -> IO ()
logTransaction party label txId = do
  putTextLn $ "[" <> show party <> "] 📝 " <> label
  putTextLn $ "    Transaction ID: " <> formatHex txId

{- |
Log signature details.

Args:
  - party: The party participant logging the signature
  - label: Description of the signature
  - sigR: The nonce component (R)
  - sigS: The scalar component (s)
-}
logSignature :: Participant -> Text -> ByteString -> ByteString -> IO ()
logSignature party label sigR sigS = do
  putTextLn $ "[" <> show party <> "] ✍ " <> label
  putTextLn $ "    R: " <> formatHex sigR
  putTextLn $ "    s: " <> formatHex sigS

{- |
Log public key information.

Args:
  - party: The party participant logging the public key
  - label: Description of the key
  - pubKey: The public key bytes
-}
logPublicKey :: Participant -> Text -> ByteString -> IO ()
logPublicKey party label pubKey = do
  putTextLn $ "[" <> show party <> "] 🔑 " <> label <> ": " <> formatHex pubKey

--------------------------------------------------------------------------------
-- Utility Functions -----------------------------------------------------------

{- |
Log a visual separator line.
-}
logSeparator :: IO ()
logSeparator = do
  putTextLn ""
  putTextLn
    "--------------------------------------------------------------------------------"
  putTextLn ""

{- |
Format ByteString as hexadecimal text with truncation for readability.

Args:
  - bytes: The bytes to format

Returns: Hexadecimal representation (truncated if > 64 chars)
-}
formatHex :: ByteString -> Text
formatHex bytes =
  let hex = TE.decodeUtf8 (Base16.encode bytes)
      maxLen = 64
      hexStr = toString hex
   in if length hexStr > maxLen
        then toText (take maxLen hexStr) <> "..."
        else hex
