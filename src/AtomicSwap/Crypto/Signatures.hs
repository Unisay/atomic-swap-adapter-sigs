{- |
Module: AtomicSwap.Crypto.Signatures
Description: Randomized EdDSA (rEdDSA) signature operations

This module implements the randomized EdDSA signature scheme as specified
in Zhu et al. (2024). Unlike deterministic Ed25519, rEdDSA includes a
random value in nonce generation, enabling adapter signature construction.

The key difference from standard Ed25519:
- Standard: r = H(sk1 || m)  (deterministic)
- rEdDSA:  r = H(sk1 || m || k)  (randomized with k)

Reference: "Adaptor signature based on randomized EdDSA in blockchain"
           Zhu et al., Digital Communications and Networks, 2024
-}
module AtomicSwap.Crypto.Signatures
  ( -- * Signing Operations
    signREdDSA
  , verifyREdDSA

    -- * Hash Functions
  , hashMessage
  , hashForChallenge
  ) where

import Crypto.ECC.Edwards25519 qualified as Curve
import Crypto.Error (CryptoFailable (..))
import Crypto.Hash (Digest, SHA512, hash)
import Crypto.Random (MonadRandom, getRandomBytes)
import Data.ByteArray (convert)
import Data.ByteString qualified as BS

import AtomicSwap.Types
  ( Ed25519PrivateKey (..)
  , PrivateKey (..)
  , PublicKey (..)
  , Signature (..)
  )

--------------------------------------------------------------------------------
-- Randomized EdDSA Signing ----------------------------------------------------

{- |
Sign a message using randomized EdDSA (rEdDSA).

Following Zhu et al. (2024) Algorithm:

1. Select random k ∈ ℤ_q*
2. Compute r = H2(sk1 || m || k) mod q
3. Compute R = r · B
4. Compute challenge: h = H2(R || pk || m)
5. Compute signature scalar: sig = r + h · sk0 mod q
6. Output σ = (sig, R)

The randomness k prevents the nonce from being deterministic, which
is essential for adapter signature security (deterministic EdDSA would
leak the adapter secret).

==== Security Note
The random value k MUST be fresh for each signature. Reusing k with
different messages allows private key recovery.

==== Example
>>> (privKey, pubKey) <- generateKeyPair
>>> sig <- signREdDSA privKey pubKey "Hello, atomic swaps!"
>>> verifyREdDSA pubKey "Hello, atomic swaps!" sig
True
-}
signREdDSA
  :: MonadRandom m
  => Ed25519PrivateKey
  -- ^ Signer's private key
  -> PublicKey
  -- ^ Signer's public key
  -> BS.ByteString
  -- ^ Message to sign
  -> m Signature
signREdDSA (Ed25519PrivateKey (PrivateKey sk0) sk1) (PublicKey pkBytes) message =
  do
    -- Step 1: Generate random k (32 bytes for Ed25519)
    k <- getRandomBytes 32

    -- Step 2: Compute nonce r = H2(sk1 || m || k) mod q
    let nonceInput = sk1 <> message <> k
        r = hashToScalar nonceInput

    -- Step 3: Compute R = r · B (scalar multiplication by base point)
    let rPoint = scalarMultBase r

    -- Step 4: Compute challenge h = H2(R || pk || m)
    let challengeInput = rPoint <> pkBytes <> message
        challenge = hashToScalar challengeInput

    -- Step 5: Compute signature scalar: sig = r + h · sk0 mod q
    let hTimesSk0 = scalarMul challenge sk0
        sig = scalarAdd r hTimesSk0

    -- Step 6: Return signature (R, sig)
    return $
      Signature
        { sigNonce = rPoint
        , sigScalar = sig
        }

--------------------------------------------------------------------------------
-- Signature Verification ------------------------------------------------------

{- |
Verify a randomized EdDSA signature.

Verification equation: sig · B = R + h · pk

Where:
- sig: signature scalar
- B: Ed25519 base point
- R: nonce point from signature
- h: challenge = H2(R || pk || m)
- pk: public key

Returns True if signature is valid, False otherwise.
-}
verifyREdDSA
  :: PublicKey
  -- ^ Signer's public key
  -> BS.ByteString
  -- ^ Message that was signed
  -> Signature
  -- ^ Signature to verify
  -> Bool
verifyREdDSA (PublicKey pkBytes) message sig =
  -- Compute challenge: h = H2(R || pk || m)
  let rPoint = sigNonce sig
      sigScalar' = sigScalar sig
      challengeInput = rPoint <> pkBytes <> message
      challenge = hashToScalar challengeInput

      -- Left side: sig · B
      lhs = scalarMultBase sigScalar'

      -- Right side: R + h · pk
      hTimesPk = scalarMultPoint challenge pkBytes
      rhs = pointAdd rPoint hTimesPk
   in lhs == rhs

--------------------------------------------------------------------------------
-- Hash Functions --------------------------------------------------------------

{- |
Hash a message to a scalar value modulo the Ed25519 group order.

This implements H2: {0,1}* → ℤ_q* from the rEdDSA specification.

Uses SHA-512 and reduces modulo the Ed25519 group order:
q = 2^252 + 27742317777372353535851937790883648493
-}
hashToScalar :: ByteString -> ByteString
hashToScalar input =
  let h = hash input :: Digest SHA512
      hashBytes = convert h :: ByteString
   in hashBytes -- Return full 64 bytes for scalarDecodeLong (it can handle variable length)

{- |
Hash a message for general purposes (SHA-512).
-}
hashMessage :: ByteString -> ByteString
hashMessage input =
  let h = hash input :: Digest SHA512
   in convert h :: ByteString

{- |
Compute challenge hash for signature.

h = H2(R || pk || m)
-}
hashForChallenge :: ByteString -> ByteString -> ByteString -> ByteString
hashForChallenge rPoint pkBytes message =
  hashToScalar (rPoint <> pkBytes <> message)

--------------------------------------------------------------------------------
-- Elliptic Curve Operations ---------------------------------------------------

-- | Scalar multiplication by base point: s · B
scalarMultBase :: BS.ByteString -> BS.ByteString
scalarMultBase scalarBytes =
  case Curve.scalarDecodeLong scalarBytes of
    CryptoPassed scalar ->
      let point = Curve.toPoint scalar -- s · B
       in Curve.pointEncode point
    CryptoFailed err -> error $ "Invalid scalar for base point multiplication: " <> show err

-- | Scalar multiplication of point: s · P
scalarMultPoint :: BS.ByteString -> BS.ByteString -> BS.ByteString
scalarMultPoint scalarBytes pointBytes =
  case (Curve.scalarDecodeLong scalarBytes, Curve.pointDecode pointBytes) of
    (CryptoPassed scalar, CryptoPassed point) ->
      let result = Curve.pointMul scalar point -- s · P
       in Curve.pointEncode result
    (CryptoFailed err, _) -> error $ "Invalid scalar: " <> show err
    (_, CryptoFailed err) -> error $ "Invalid point: " <> show err

-- | Point addition: P + Q
pointAdd :: BS.ByteString -> BS.ByteString -> BS.ByteString
pointAdd p1Bytes p2Bytes =
  case (Curve.pointDecode p1Bytes, Curve.pointDecode p2Bytes) of
    (CryptoPassed p1, CryptoPassed p2) ->
      let result = Curve.pointAdd p1 p2 -- P + Q
       in Curve.pointEncode result
    (CryptoFailed err, _) -> error $ "Invalid point P: " <> show err
    (_, CryptoFailed err) -> error $ "Invalid point Q: " <> show err

-- | Scalar addition modulo group order: (a + b) mod q
scalarAdd :: BS.ByteString -> BS.ByteString -> BS.ByteString
scalarAdd s1Bytes s2Bytes =
  case (Curve.scalarDecodeLong s1Bytes, Curve.scalarDecodeLong s2Bytes) of
    (CryptoPassed s1, CryptoPassed s2) ->
      let result = Curve.scalarAdd s1 s2 -- (a + b) mod q
       in Curve.scalarEncode result
    (CryptoFailed err, _) -> error $ "Invalid scalar s1: " <> show err
    (_, CryptoFailed err) -> error $ "Invalid scalar s2: " <> show err

-- | Scalar multiplication modulo group order: (a · b) mod q
scalarMul :: BS.ByteString -> BS.ByteString -> BS.ByteString
scalarMul s1Bytes s2Bytes =
  case (Curve.scalarDecodeLong s1Bytes, Curve.scalarDecodeLong s2Bytes) of
    (CryptoPassed s1, CryptoPassed s2) ->
      let result = Curve.scalarMul s1 s2 -- (a · b) mod q
       in Curve.scalarEncode result
    (CryptoFailed err, _) -> error $ "Invalid scalar s1: " <> show err
    (_, CryptoFailed err) -> error $ "Invalid scalar s2: " <> show err
