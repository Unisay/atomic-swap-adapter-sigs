{- |
Module: AtomicSwap.Blockchain.Transaction
Description: UTXO transaction logic

This module implements UTXO-based transaction construction and verification.
Transactions consume input UTXOs and create new output UTXOs, following the
Bitcoin-style unspent transaction output model.
-}
module AtomicSwap.Blockchain.Transaction
  ( -- * Transaction Construction
    buildTransaction
  , hashTransaction

    -- * Transaction Verification
  , verifyTransaction
  , verifyTransactionSignatures
  ) where

import Crypto.Hash (Digest, SHA256, hash)
import Data.Bits (shiftR)
import Data.ByteArray (convert)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map

import AtomicSwap.Blockchain.Types (LedgerState (..))
import AtomicSwap.Crypto.Signatures (verifyREdDSA)
import AtomicSwap.Types
  ( Output (..)
  , PublicKey (..)
  , Transaction (..)
  , TxId (..)
  , UTXO (..)
  )

--------------------------------------------------------------------------------
-- Transaction Construction ----------------------------------------------------

{- |
Build a transaction from inputs and outputs.

The transaction is created without signatures initially. Signatures must be
added separately using the transaction hash.
-}
buildTransaction :: [UTXO] -> [Output] -> Transaction
buildTransaction inputs outputs =
  Transaction
    { txInputs = inputs
    , txOutputs = outputs
    , txSignatures = []
    }

{- |
Hash a transaction for signing.

The hash includes all inputs and outputs but NOT the signatures themselves,
following standard practice to prevent circular dependencies.
-}
hashTransaction :: Transaction -> ByteString
hashTransaction tx =
  let inputBytes = foldMap encodeUTXO (txInputs tx)
      outputBytes = foldMap encodeOutput (txOutputs tx)
      digest :: Digest SHA256 = hash (inputBytes <> outputBytes)
   in convert digest

-- | Encode UTXO for hashing
encodeUTXO :: UTXO -> ByteString
encodeUTXO utxo =
  let TxId txId = utxoTxId utxo
      PublicKey owner = utxoOwner utxo
      indexBytes = BS.pack [fromIntegral (utxoIndex utxo)]
      amountBytes = BS.pack (word64ToBytes (utxoAmount utxo))
   in txId <> indexBytes <> amountBytes <> owner

-- | Encode output for hashing
encodeOutput :: Output -> ByteString
encodeOutput out =
  let PublicKey owner = outputOwner out
      amountBytes = BS.pack (word64ToBytes (outputAmount out))
   in amountBytes <> owner

-- | Convert Word64 to bytes (little-endian, 8 bytes)
word64ToBytes :: Word64 -> [Word8]
word64ToBytes n =
  [ fromIntegral n
  , fromIntegral (n `shiftR` 8)
  , fromIntegral (n `shiftR` 16)
  , fromIntegral (n `shiftR` 24)
  , fromIntegral (n `shiftR` 32)
  , fromIntegral (n `shiftR` 40)
  , fromIntegral (n `shiftR` 48)
  , fromIntegral (n `shiftR` 56)
  ]

--------------------------------------------------------------------------------
-- Transaction Verification ----------------------------------------------------

{- |
Verify a transaction against the blockchain state.

Checks:
1. All inputs exist as unspent UTXOs
2. All inputs have valid signatures
3. Total input amount ≥ total output amount (no inflation)
4. No double-spending (all inputs are unique)
-}
verifyTransaction :: Transaction -> LedgerState -> Either Text ()
verifyTransaction tx ledger = do
  -- Check all inputs exist
  forM_ (txInputs tx) \input -> do
    let txId = utxoTxId input
    case Map.lookup txId (ledgerUTXOs ledger) of
      Nothing -> Left $ "Input UTXO not found: " <> show txId
      Just utxos ->
        unless (input `elem` utxos) $
          Left "Input UTXO index mismatch"

  -- Verify signatures
  let txHash = hashTransaction tx
  verifyTransactionSignatures tx txHash

  -- Check conservation of funds
  let totalInput = sum (map utxoAmount (txInputs tx))
      totalOutput = sum (map outputAmount (txOutputs tx))
  when (totalOutput > totalInput) $
    Left "Output amount exceeds input amount"

  -- Check no double-spending (all inputs unique)
  let inputIds = map (\u -> (utxoTxId u, utxoIndex u)) (txInputs tx)
  when (length inputIds /= length (ordNub inputIds)) $
    Left "Double-spending detected"

{- |
Verify all transaction signatures against the transaction hash.

Each signature must verify against the corresponding input's owner public key.
-}
verifyTransactionSignatures
  :: Transaction
  -> ByteString
  -> Either Text ()
verifyTransactionSignatures tx txHash = do
  -- Check we have exactly one signature per input
  let numInputs = length (txInputs tx)
      numSigs = length (txSignatures tx)
  when (numSigs /= numInputs) $
    Left $
      "Signature count mismatch: "
        <> show numSigs
        <> " signatures for "
        <> show numInputs
        <> " inputs"

  -- Verify each signature
  forM_ (zip (txInputs tx) (txSignatures tx)) \(input, sig) -> do
    let pubKey = utxoOwner input
    unless (verifyREdDSA pubKey txHash sig) $
      Left $
        "Invalid signature for input: " <> show (utxoTxId input)
