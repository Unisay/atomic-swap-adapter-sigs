{- |
Module: AtomicSwap.Crypto.Keys
Description: Ed25519 key generation and management for rEdDSA

This module implements key generation following the randomized EdDSA
specification from Zhu et al. (2024). Keys are generated according to
the Ed25519/RFC 8032 standard with the modifications needed for
randomized signatures.

Reference: "Adaptor signature based on randomized EdDSA in blockchain"
           Zhu et al., Digital Communications and Networks, 2024
-}
module AtomicSwap.Crypto.Keys
  ( -- * Key Generation
    generateKeyPair

    -- * Key Derivation
  , derivePublicKey

    -- * Utilities
  , exportPrivateKey
  , exportPublicKey
  , importPrivateKey
  , importPublicKey
  ) where

import Crypto.ECC.Edwards25519 qualified as Curve
import Crypto.Error (CryptoFailable (..))
import Crypto.Hash (Digest, SHA512, hash)
import Crypto.Random (MonadRandom, getRandomBytes)
import Data.Bits ((.&.), (.|.))
import Data.ByteArray (convert)
import Data.ByteString qualified as BS

import AtomicSwap.Types
  ( Ed25519PrivateKey (..)
  , PrivateKey (..)
  , PublicKey (..)
  )

--------------------------------------------------------------------------------
-- Key Generation --------------------------------------------------------------

{- |
Generate a fresh Ed25519 keypair for rEdDSA signatures.

This follows the rEdDSA key generation from Zhu et al. (2024):

1. Select random b-bit string t as private key (b=256 for Ed25519)
2. Compute H1(t) = (h0, h1, ..., h_511) using SHA-512
3. Calculate sk0 = 2^n + Σ_{c≤i<n} 2^i · h_i (clamping)
4. Calculate public key: pk = sk0 · B
5. Define sk1 = (h_256, ..., h_511) for nonce generation

The private key is split into two parts:
- sk0: Used for signing (clamped scalar)
- sk1: Used for nonce generation (randomness)

==== Example
>>> (privKey, pubKey) <- generateKeyPair
>>> -- Can now use for rEdDSA signing
-}
generateKeyPair :: MonadRandom m => m (Ed25519PrivateKey, PublicKey)
generateKeyPair = do
  -- Step 1: Generate random 32-byte (256-bit) seed
  seed <- getRandomBytes 32

  -- Step 2: Hash the seed with SHA-512 (H1 function)
  let hashBytes = hashSHA512 seed

  -- Step 3: Clamp the first 32 bytes to get sk0
  let sk0Bytes = clampScalar (BS.take 32 hashBytes)

  -- Step 4: Second 32 bytes become sk1 (nonce seed)
  let sk1Bytes = BS.drop 32 hashBytes

  -- Step 5: Derive public key from sk0
  let pubKey = derivePublicKeyFromScalar sk0Bytes

  -- Step 6: Package private key
  let privKey =
        Ed25519PrivateKey
          { ed25519SK0 = PrivateKey sk0Bytes
          , ed25519SK1 = sk1Bytes
          }

  return (privKey, pubKey)

--------------------------------------------------------------------------------
-- Key Derivation --------------------------------------------------------------

{- |
Derive public key from private key scalar.

Computes: pk = sk0 · B

Where B is the Ed25519 base point.
-}
derivePublicKey :: Ed25519PrivateKey -> PublicKey
derivePublicKey (Ed25519PrivateKey (PrivateKey sk0) _) =
  derivePublicKeyFromScalar sk0

-- | Internal: Derive public key from scalar bytes
derivePublicKeyFromScalar :: ByteString -> PublicKey
derivePublicKeyFromScalar sk0Bytes =
  case Curve.scalarDecodeLong sk0Bytes of
    CryptoPassed sk0Scalar ->
      let pubKeyPoint = Curve.toPoint sk0Scalar -- pk = sk0 · B
          pubKeyBytes = Curve.pointEncode pubKeyPoint
       in PublicKey pubKeyBytes
    CryptoFailed err -> error $ "Invalid secret key scalar: " <> show err

--------------------------------------------------------------------------------
-- Scalar Clamping (Ed25519 Standard) ------------------------------------------

{- |
Clamp a scalar according to Ed25519 specification (RFC 8032).

The clamping operation:
1. Set bits 0, 1, 2 to 0 (multiple of 8)
2. Set bit 254 to 1
3. Set bit 255 to 0

This ensures the scalar is in the correct range and avoids certain
attacks related to small-order points.

Reference: RFC 8032 Section 5.1.5
-}
clampScalar :: ByteString -> ByteString
clampScalar bs
  | BS.length bs /= 32 = error "Scalar must be 32 bytes"
  | otherwise = BS.pack clamped
  where
    bytes = BS.unpack bs

    -- Safely extract bytes
    byte0 = case viaNonEmpty head bytes of
      Just b -> b .&. 0xF8 -- Clamp first byte: clear bits 0, 1, 2
      Nothing -> error "Empty byte array"

    byteLast = case viaNonEmpty last bytes of
      Just b -> (b .&. 0x7F) .|. 0x40 -- Clamp last byte: clear bit 255, set bit 254
      Nothing -> error "Empty byte array"

    middleBytes = case viaNonEmpty tail bytes of
      Just bs' -> take 30 bs'
      Nothing -> error "Empty byte array"

    clamped = byte0 : middleBytes ++ [byteLast]

--------------------------------------------------------------------------------
-- Helper Functions ------------------------------------------------------------

-- | Hash using SHA-512
hashSHA512 :: ByteString -> ByteString
hashSHA512 input = convert (hash input :: Digest SHA512)

--------------------------------------------------------------------------------
-- Import/Export ---------------------------------------------------------------

{- |
Export private key to raw bytes for serialization.

__Security Warning__: Private keys should be handled carefully and
never logged or transmitted insecurely.
-}
exportPrivateKey :: Ed25519PrivateKey -> (ByteString, ByteString)
exportPrivateKey (Ed25519PrivateKey (PrivateKey sk0) sk1) = (sk0, sk1)

{- |
Export public key to 32-byte compressed format.
-}
exportPublicKey :: PublicKey -> ByteString
exportPublicKey (PublicKey bytes) = bytes

{- |
Import private key from raw bytes.

This reconstructs an Ed25519PrivateKey from the two components.
-}
importPrivateKey :: ByteString -> ByteString -> Ed25519PrivateKey
importPrivateKey sk0 sk1 = Ed25519PrivateKey (PrivateKey sk0) sk1

{- |
Import public key from 32-byte compressed format.
-}
importPublicKey :: ByteString -> PublicKey
importPublicKey = PublicKey
