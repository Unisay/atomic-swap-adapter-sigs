{- |
Module: AtomicSwap.Crypto.NIZK
Description: NIZK proofs for discrete logarithm relation

This module implements non-interactive zero-knowledge proofs for the
discrete logarithm relation: (Y, y) where Y = y·B.

The proof uses the Schnorr sigma protocol made non-interactive via
the Fiat-Shamir transform. This is required by the Zhu et al. (2024)
rEdDSA adapter signature scheme.

Protocol:
1. Prover generates random r, computes R = r·B
2. Compute challenge c = H(Y || R) (Fiat-Shamir)
3. Compute response s = r + c·y
4. Proof π = (R, s)

Verification:
1. Recompute challenge c = H(Y || R)
2. Check s·B = R + c·Y

This proves knowledge of y without revealing y itself.
-}
module AtomicSwap.Crypto.NIZK
  ( -- * NIZK Proof Operations
    proveDiscreteLog
  , verifyDiscreteLog
  ) where

import Crypto.ECC.Edwards25519 qualified as Curve
import Crypto.Error (CryptoFailable (..))
import Crypto.Hash (Digest, SHA512, hash)
import Crypto.Random (MonadRandom, getRandomBytes)
import Data.ByteArray (convert)
import Data.ByteString qualified as BS

import AtomicSwap.Types
  ( AdapterPoint (..)
  , AdapterSecret (..)
  , NIZKProof (..)
  )

--------------------------------------------------------------------------------
-- NIZK Proof Generation -------------------------------------------------------

{- |
Generate a NIZK proof of knowledge of discrete logarithm.

Proves: "I know y such that Y = y·B"

Uses Schnorr's sigma protocol with Fiat-Shamir transform:

1. Generate random r ∈ ℤ_q*
2. Compute R = r·B
3. Compute challenge c = H(Y || R)  (Fiat-Shamir heuristic)
4. Compute response s = r + c·y mod q
5. Output proof π = (R, s)

The proof can be verified without revealing the witness y.

==== Example
>>> secret <- generateAdapterSecret
>>> let commitment = generateAdapterCommitment secret
>>> proof <- proveDiscreteLog secret commitment
>>> verifyDiscreteLog commitment proof
True
-}
proveDiscreteLog
  :: MonadRandom m
  => AdapterSecret
  -- ^ Witness (adapter secret y)
  -> AdapterPoint
  -- ^ Statement (adapter point Y = y·B)
  -> m NIZKProof
proveDiscreteLog (AdapterSecret yBytes) (AdapterPoint yPoint) = do
  -- Step 1: Generate random nonce r
  rBytes' <- getRandomBytes 64 -- Use 64 bytes for better distribution
  let rBytes = rBytes' :: ByteString

  -- Decode as scalar
  case Curve.scalarDecodeLong rBytes of
    CryptoFailed err -> error $ "Failed to generate random scalar: " <> show err
    CryptoPassed r -> do
      -- Step 2: Compute R = r·B
      let rPoint = Curve.toPoint r
          rPointBytes = Curve.pointEncode rPoint

      -- Step 3: Compute challenge c = H(Y || R)
      let challengeInput = yPoint <> rPointBytes
          challengeHash = hash challengeInput :: Digest SHA512
          challengeBytes = convert challengeHash :: ByteString

      case Curve.scalarDecodeLong challengeBytes of
        CryptoFailed err -> error $ "Failed to decode challenge: " <> show err
        CryptoPassed c -> do
          -- Step 4: Compute response s = r + c·y mod q
          case Curve.scalarDecodeLong yBytes of
            CryptoFailed err -> error $ "Invalid adapter secret: " <> show err
            CryptoPassed y -> do
              let cTimesY = Curve.scalarMul c y
                  s = Curve.scalarAdd r cTimesY
                  sBytes = Curve.scalarEncode s

              -- Step 5: Package proof (R, s)
              let proofBytes = rPointBytes <> sBytes -- 32 + 32 = 64 bytes
              return $ NIZKProof proofBytes

--------------------------------------------------------------------------------
-- NIZK Proof Verification -----------------------------------------------------

{- |
Verify a NIZK proof of knowledge of discrete logarithm.

Verifies: Proof that prover knows y such that Y = y·B

Verification equation: s·B = R + c·Y

Where:
- s: response scalar from proof
- B: Ed25519 base point
- R: commitment point from proof
- c: challenge = H(Y || R)
- Y: statement (public adapter point)

Returns True if proof is valid, False otherwise.
-}
verifyDiscreteLog
  :: AdapterPoint
  -- ^ Statement (adapter point Y)
  -> NIZKProof
  -- ^ Proof π = (R, s)
  -> Bool
verifyDiscreteLog (AdapterPoint yPoint) (NIZKProof proofBytes) =
  -- Parse proof: first 32 bytes = R, next 32 bytes = s
  let rPointBytes = BS.take 32 proofBytes
      sBytes = BS.take 32 (BS.drop 32 proofBytes)
   in case ( Curve.pointDecode rPointBytes
           , Curve.scalarDecodeLong sBytes
           , Curve.pointDecode yPoint
           ) of
        (CryptoPassed rPoint, CryptoPassed s, CryptoPassed yPointDecoded) -> do
          -- Recompute challenge c = H(Y || R)
          let challengeInput = yPoint <> rPointBytes
              challengeHash = hash challengeInput :: Digest SHA512
              challengeBytes = convert challengeHash :: ByteString

          case Curve.scalarDecodeLong challengeBytes of
            CryptoPassed c -> do
              -- Verify: s·B = R + c·Y
              let lhs = Curve.toPoint s -- s·B
                  cTimesY = Curve.pointMul c yPointDecoded -- c·Y
                  rhs = Curve.pointAdd rPoint cTimesY -- R + c·Y
               in lhs == rhs
            CryptoFailed _ -> False
        _ -> False
