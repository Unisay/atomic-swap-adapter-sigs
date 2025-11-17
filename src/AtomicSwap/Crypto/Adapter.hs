{- |
Module: AtomicSwap.Crypto.Adapter
Description: Adapter signature construction for rEdDSA

This module implements adapter signatures on top of randomized EdDSA,
following the construction from Zhu et al. (2024).

Adapter signatures enable atomic swaps by cryptographically linking two
transactions. The key operations are:

1. preSign: Create adapted pre-signature (without adapter secret)
2. preVerify: Verify adapted pre-signature is valid
3. Adapt: Complete pre-signature by adding adapter secret
4. Extract: Recover adapter secret from pre-signature and complete signature

Reference: "Adaptor signature based on randomized EdDSA in blockchain"
           Zhu et al., Digital Communications and Networks, 2024
-}
module AtomicSwap.Crypto.Adapter
  ( -- * Adapter Secret Generation
    generateAdapterSecret
  , generateAdapterCommitment

    -- * Adapter Signature Operations
  , preSignREdDSA
  , preVerifyREdDSA
  , adaptSignature
  , extractAdapterSecret
  ) where

import Crypto.ECC.Edwards25519 qualified as Curve
import Crypto.Error (CryptoFailable (..))
import Crypto.Hash (Digest, SHA512, hash)
import Crypto.Number.Serialize.LE (i2ospOf, os2ip) -- Little-endian for Ed25519!
import Crypto.Random (MonadRandom, getRandomBytes)
import Data.ByteArray (convert)

import AtomicSwap.Types
  ( AdaptedSignature (..)
  , AdapterPoint (..)
  , AdapterSecret (..)
  , Ed25519PrivateKey (..)
  , NIZKProof (..)
  , PrivateKey (..)
  , PublicKey (..)
  , Signature (..)
  )

--------------------------------------------------------------------------------
-- Adapter Secret Generation ---------------------------------------------------

{- |
Generate a random adapter secret.

This is done by Alice (the initiator) to create the cryptographic link
between both transactions in the atomic swap.

The adapter secret is a random scalar in ℤ_q* where q is the Ed25519
group order.
-}
generateAdapterSecret :: MonadRandom m => m AdapterSecret
generateAdapterSecret = do
  -- Generate random 64 bytes for scalarDecodeLong
  randomBytes' <- getRandomBytes 64
  let randomBytes = randomBytes' :: ByteString

  -- Decode and re-encode to get canonical 32-byte scalar
  case Curve.scalarDecodeLong randomBytes of
    CryptoPassed scalar ->
      let scalarBytes = Curve.scalarEncode scalar -- Always 32 bytes
       in return $ AdapterSecret scalarBytes
    CryptoFailed err -> error $ "Failed to generate adapter secret: " <> show err

{- |
Generate adapter commitment from adapter secret.

Computes: Y = y · B

Where:
- y: adapter secret (scalar)
- B: Ed25519 base point
- Y: adapter point (commitment)

This commitment Y is shared publicly with Bob and used in both
adapted pre-signatures.
-}
generateAdapterCommitment :: AdapterSecret -> AdapterPoint
generateAdapterCommitment (AdapterSecret yBytes) =
  case Curve.scalarDecodeLong yBytes of
    CryptoPassed y ->
      let yPoint = Curve.toPoint y -- Y = y · B
       in AdapterPoint (Curve.pointEncode yPoint)
    CryptoFailed err ->
      error $ "Invalid adapter secret: " <> show err

--------------------------------------------------------------------------------
-- Adapter Signature Creation --------------------------------------------------

{- |
Create an adapted pre-signature for rEdDSA.

Following Zhu et al. (2024) Algorithm 4 (pSign):

1. Select random k ∈ ℤ_q*
2. Compute r = H2(sk1 || m || k) mod q
3. Compute R_pre = r · B
4. Compute R_sign = R_pre + Y  ← KEY: Add adapter point!
5. Compute challenge: h = H2(R_sign || pk || m)
6. Compute pre-signature: sig_tilde = r + h · sk0 mod q
   (Note: does NOT include y!)
7. Output: (sig_tilde, R_sign, π)

The pre-signature is verifiable but cannot be used as a valid signature
until the adapter secret y is added.

==== Example
>>> (privKey, pubKey) <- generateKeyPair
>>> secret <- generateAdapterSecret
>>> let commitment = generateAdapterCommitment secret
>>> preSig <- preSignREdDSA privKey pubKey "message" commitment proof
>>> preVerifyREdDSA pubKey "message" commitment preSig
True
-}
preSignREdDSA
  :: MonadRandom m
  => Ed25519PrivateKey
  -- ^ Signer's private key
  -> PublicKey
  -- ^ Signer's public key
  -> ByteString
  -- ^ Message to sign
  -> AdapterPoint
  -- ^ Adapter point Y = y·B
  -> NIZKProof
  -- ^ NIZK proof for Y
  -> m AdaptedSignature
preSignREdDSA (Ed25519PrivateKey (PrivateKey sk0) sk1) (PublicKey pkBytes) message (AdapterPoint yPoint) _proof =
  do
    -- Step 1: Generate random k
    k <- getRandomBytes 32

    -- Step 2: Compute nonce r = H2(sk1 || m || k) mod q
    let nonceInput = sk1 <> message <> k
        r = hashToScalar nonceInput

    -- Step 3: Compute R_pre = r · B
    let rPrePoint = scalarMultBase r

    -- Step 4: Compute R_sign = R_pre + Y (add adapter point!)
    let rSignPoint = pointAdd rPrePoint yPoint

    -- Step 5: Compute challenge h = H2(R_sign || pk || m)
    let challengeInput = rSignPoint <> pkBytes <> message
        challenge = hashToScalar challengeInput

    -- Step 6: Compute pre-signature sig_tilde = r + h · sk0 (WITHOUT y!)
    let hTimesSk0 = scalarMul challenge sk0
        sigTilde = scalarAdd r hTimesSk0

    -- Step 7: Return adapted pre-signature
    return $
      AdaptedSignature
        { adaptedNonce = rSignPoint
        , adaptedScalar = sigTilde
        }

--------------------------------------------------------------------------------
-- Adapter Signature Verification ---------------------------------------------

{- |
Verify an adapted pre-signature for rEdDSA.

Following Zhu et al. (2024) Algorithm 5 (pVerify):

1. Parse σ_tilde = (sig_tilde, R_sign, π)
2. Compute R' = R_sign - Y  ← Remove adapter point
3. Compute h = H2(R_sign || pk || m)
4. Verify NIZK proof: Verify_zk(Y, π) = 1
5. Check equation: sig_tilde · B = R' + h · pk
6. Output 1 if valid, 0 otherwise

The verification confirms the pre-signature is correctly formed and will
become a valid signature once the adapter secret y is added.

==== Verification Equation
Why does sig_tilde · B = R' + h · pk work?

  sig_tilde · B = (r + h·sk0) · B       [by construction]
                = r·B + h·sk0·B
                = r·B + h·pk              [pk = sk0·B]
                = (R_sign - Y) + h·pk     [R_sign = r·B + Y]
                = R' + h·pk  ✓
-}
preVerifyREdDSA
  :: PublicKey
  -- ^ Signer's public key
  -> ByteString
  -- ^ Message that was signed
  -> AdapterPoint
  -- ^ Adapter point Y
  -> AdaptedSignature
  -- ^ Adapted pre-signature
  -> NIZKProof
  -- ^ NIZK proof
  -> Bool
preVerifyREdDSA (PublicKey pkBytes) message (AdapterPoint yPoint) preSig _proof =
  -- TODO: Verify NIZK proof (for now, skip)
  let rSign = adaptedNonce preSig
      sigTilde' = adaptedScalar preSig

      -- Step 2: Compute R' = R_sign - Y
      rPrime = pointSubtract rSign yPoint

      -- Step 3: Compute challenge h = H2(R_sign || pk || m)
      challengeInput = rSign <> pkBytes <> message
      challenge = hashToScalar challengeInput

      -- Step 5: Verify equation: sig_tilde · B = R' + h · pk
      lhs = scalarMultBase sigTilde'
      hTimesPk = scalarMultPoint challenge pkBytes
      rhs = pointAdd rPrime hTimesPk
   in lhs == rhs

--------------------------------------------------------------------------------
-- Adapter Signature Completion ------------------------------------------------

{- |
Complete an adapted pre-signature by adding the adapter secret.

Following Zhu et al. (2024) Algorithm 6 (Adapt):

Given pre-signature (sig_tilde, R_sign) and adapter secret y:
1. Compute sig = sig_tilde + y
2. Return (R_sign, sig)

Now (R_sign, sig) is a valid rEdDSA signature!

==== Why This Works
  sig · B = (sig_tilde + y) · B
          = sig_tilde · B + y · B
          = R' + h·pk + Y           [from pre-sig verification]
          = (R_sign - Y) + h·pk + Y
          = R_sign + h·pk  ✓
-}
adaptSignature
  :: AdaptedSignature
  -- ^ Adapted pre-signature
  -> AdapterSecret
  -- ^ Adapter secret
  -> Signature
adaptSignature preSig (AdapterSecret yBytes) =
  let rSign = adaptedNonce preSig
      sigTilde' = adaptedScalar preSig

      -- Compute sig = sig_tilde + y
      sig = scalarAdd sigTilde' yBytes
   in Signature
        { sigNonce = rSign
        , sigScalar = sig
        }

--------------------------------------------------------------------------------
-- Adapter Secret Extraction ---------------------------------------------------

{- |
Extract the adapter secret from a pre-signature and complete signature.

Following Zhu et al. (2024) Algorithm 7 (Ext):

Given:
- Pre-signature: (sig_tilde, R_sign, π)
- Complete signature: (R_sign, sig)

Extract: y = sig - sig_tilde

This is THE KEY OPERATION that makes atomic swaps work!
When Alice publishes her complete signature, Bob can extract the adapter
secret and use it to complete his own signature.

==== Example
>>> preSig <- preSignREdDSA aliceKey alicePub msg commitment proof
>>> let completeSig = adaptSignature preSig secret
>>> let extracted = extractAdapterSecret preSig completeSig
>>> extracted == secret
True
-}
extractAdapterSecret
  :: AdaptedSignature
  -- ^ Adapted pre-signature
  -> Signature
  -- ^ Complete signature (after Adapt)
  -> AdapterSecret
extractAdapterSecret preSig completeSig =
  let sigTilde' = adaptedScalar preSig
      sig = sigScalar completeSig

      -- Extract: y = sig - sig_tilde
      y = scalarSubtract sig sigTilde'
   in AdapterSecret y

--------------------------------------------------------------------------------
-- Helper Functions ------------------------------------------------------------

-- | Point subtraction: P - Q (implemented as P + (-Q))
pointSubtract :: ByteString -> ByteString -> ByteString
pointSubtract p1Bytes p2Bytes =
  case (Curve.pointDecode p1Bytes, Curve.pointDecode p2Bytes) of
    (CryptoPassed p1, CryptoPassed p2) ->
      let negP2 = Curve.pointNegate p2
          result = Curve.pointAdd p1 negP2 -- P - Q = P + (-Q)
       in Curve.pointEncode result
    (CryptoFailed err, _) -> error $ "Invalid point P: " <> show err
    (_, CryptoFailed err) -> error $ "Invalid point Q: " <> show err

-- | Scalar subtraction modulo group order: (a - b) mod l
scalarSubtract :: ByteString -> ByteString -> ByteString
scalarSubtract s1Bytes s2Bytes =
  -- Ed25519 group order (prime order of base point)
  -- l = 2^252 + 27742317777372353535851937790883648493
  let groupOrder = 2 ^ (252 :: Integer) + 27742317777372353535851937790883648493

      -- Convert scalars to integers
      s1Int = os2ip s1Bytes
      s2Int = os2ip s2Bytes

      -- Compute (s1 - s2) mod l
      result = (s1Int - s2Int) `mod` groupOrder

      -- Convert back to 32-byte little-endian
      resultBytes = case i2ospOf 32 result of
        Just bytes -> bytes
        Nothing -> error "Scalar subtraction result too large"
   in resultBytes

-- | Hash to scalar (H2 function from Zhu et al.)
hashToScalar :: ByteString -> ByteString
hashToScalar input =
  let h = hash input :: Digest SHA512
      hashBytes = convert h :: ByteString
   in hashBytes -- SHA-512 produces 64 bytes, we'll use first 32 implicitly via scalarDecodeLong

-- | Scalar addition (from Signatures module, re-exported for clarity)
scalarAdd :: ByteString -> ByteString -> ByteString
scalarAdd s1Bytes s2Bytes =
  case (Curve.scalarDecodeLong s1Bytes, Curve.scalarDecodeLong s2Bytes) of
    (CryptoPassed s1, CryptoPassed s2) ->
      Curve.scalarEncode (Curve.scalarAdd s1 s2)
    (CryptoFailed err, _) -> error $ "Invalid scalar: " <> show err
    (_, CryptoFailed err) -> error $ "Invalid scalar: " <> show err

-- | Scalar multiplication
scalarMul :: ByteString -> ByteString -> ByteString
scalarMul s1Bytes s2Bytes =
  case (Curve.scalarDecodeLong s1Bytes, Curve.scalarDecodeLong s2Bytes) of
    (CryptoPassed s1, CryptoPassed s2) ->
      Curve.scalarEncode (Curve.scalarMul s1 s2)
    (CryptoFailed err, _) -> error $ "Invalid scalar: " <> show err
    (_, CryptoFailed err) -> error $ "Invalid scalar: " <> show err

-- | Scalar multiplication by base point
scalarMultBase :: ByteString -> ByteString
scalarMultBase scalarBytes =
  case Curve.scalarDecodeLong scalarBytes of
    CryptoPassed scalar ->
      Curve.pointEncode (Curve.toPoint scalar)
    CryptoFailed err -> error $ "Invalid scalar: " <> show err

-- | Point addition
pointAdd :: ByteString -> ByteString -> ByteString
pointAdd p1Bytes p2Bytes =
  case (Curve.pointDecode p1Bytes, Curve.pointDecode p2Bytes) of
    (CryptoPassed p1, CryptoPassed p2) ->
      Curve.pointEncode (Curve.pointAdd p1 p2)
    (CryptoFailed err, _) -> error $ "Invalid point: " <> show err
    (_, CryptoFailed err) -> error $ "Invalid point: " <> show err

-- | Scalar multiplication of point
scalarMultPoint :: ByteString -> ByteString -> ByteString
scalarMultPoint scalarBytes pointBytes =
  case (Curve.scalarDecodeLong scalarBytes, Curve.pointDecode pointBytes) of
    (CryptoPassed scalar, CryptoPassed point) ->
      Curve.pointEncode (Curve.pointMul scalar point)
    (CryptoFailed err, _) -> error $ "Invalid scalar: " <> show err
    (_, CryptoFailed err) -> error $ "Invalid point: " <> show err
