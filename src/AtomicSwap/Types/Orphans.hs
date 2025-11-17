{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module: AtomicSwap.Types.Orphans
Description: Orphan instances for core types

This module contains orphan FromJSON/ToJSON instances for ByteString-based
newtypes. All instances use hex encoding for human-readable JSON output.

Orphan instances are isolated here for easy auditing and maintenance.
-}
module AtomicSwap.Types.Orphans () where

import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.Aeson qualified as Aeson
import Data.ByteString.Base16 qualified as Base16
import Data.Text.Encoding qualified as TE

import AtomicSwap.Types
  ( Output (..)
  , PublicKey (..)
  , Signature (..)
  , Transaction (..)
  , TxId (..)
  , UTXO (..)
  )

--------------------------------------------------------------------------------
-- JSON Instances for ByteString Newtypes -------------------------------------

-- | Hex-encoded JSON for PublicKey
instance ToJSON PublicKey where
  toJSON (PublicKey bs) = toJSON (TE.decodeUtf8 (Base16.encode bs))

instance FromJSON PublicKey where
  parseJSON = withText "PublicKey" \t ->
    case Base16.decode (TE.encodeUtf8 t) of
      Right bs -> pure (PublicKey bs)
      Left _ -> fail "Invalid hex encoding for PublicKey"

-- | Hex-encoded JSON for TxId
instance ToJSON TxId where
  toJSON (TxId bs) = toJSON (TE.decodeUtf8 (Base16.encode bs))

instance FromJSON TxId where
  parseJSON = withText "TxId" \t ->
    case Base16.decode (TE.encodeUtf8 t) of
      Right bs -> pure (TxId bs)
      Left _ -> fail "Invalid hex encoding for TxId"

-- | Structured JSON for Signature (nonce + scalar fields)
instance ToJSON Signature where
  toJSON sig =
    Aeson.object
      [ "nonce" Aeson..= TE.decodeUtf8 (Base16.encode (sigNonce sig))
      , "scalar" Aeson..= TE.decodeUtf8 (Base16.encode (sigScalar sig))
      ]

instance FromJSON Signature where
  parseJSON = Aeson.withObject "Signature" \o -> do
    nonceHex <- o Aeson..: "nonce"
    scalarHex <- o Aeson..: "scalar"
    case ( Base16.decode (TE.encodeUtf8 nonceHex)
         , Base16.decode (TE.encodeUtf8 scalarHex)
         ) of
      (Right nonce, Right scalar) -> pure $ Signature nonce scalar
      _ -> fail "Invalid hex encoding for Signature"

--------------------------------------------------------------------------------
-- JSON Instances for Composite Types -----------------------------------------

-- | Standard Aeson derivation for UTXO (contains only JSON-serializable types)
instance ToJSON UTXO

instance FromJSON UTXO

-- | Standard Aeson derivation for Output
instance ToJSON Output

instance FromJSON Output

-- | Standard Aeson derivation for Transaction
instance ToJSON Transaction

instance FromJSON Transaction
