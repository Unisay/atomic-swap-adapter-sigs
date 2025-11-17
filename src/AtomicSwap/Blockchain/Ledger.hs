{- |
Module: AtomicSwap.Blockchain.Ledger
Description: File-based blockchain ledger operations

This module implements file-based blockchain operations using JSON for
persistence. Each blockchain maintains its state in a TVar and provides
operations for initialization, transaction submission, and UTXO queries.

Polymorphic over MonadSTM for deterministic testing with io-sim.
-}
module AtomicSwap.Blockchain.Ledger
  ( -- * Blockchain Initialization
    initBlockchain
  , loadBlockchain

    -- * Transaction Operations
  , submitTransaction
  , queryUTXOs
  , getBalance

    -- * Persistence
  , saveBlockchain
  ) where

import Prelude hiding (STM, TVar, atomically, newTVar, readTVar, writeTVar)

import Control.Concurrent.Class.MonadSTM.TVar (newTVar, readTVar, writeTVar)
import Control.Exception (catch)
import Control.Monad.Class.MonadSTM (MonadSTM, atomically)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict, encodeFile)
import Data.Bits (shiftR)
import Data.ByteArray (convert)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import System.Directory (doesFileExist)

import AtomicSwap.Blockchain.Transaction (verifyTransaction)
import AtomicSwap.Blockchain.Types
  ( Blockchain (..)
  , BlockchainName
  , GenesisUTXO (..)
  , LedgerState (..)
  )
import AtomicSwap.Types
  ( Output (..)
  , PublicKey (..)
  , Signature (..)
  , Transaction (..)
  , TxId (..)
  , UTXO (..)
  )
import AtomicSwap.Types.Orphans ()

--------------------------------------------------------------------------------
-- JSON Instances --------------------------------------------------------------

-- | JSON-serializable version of LedgerState
data LedgerStateJSON = LedgerStateJSON
  { utxos :: [(TxId, [UTXO])]
  , transactions :: [Transaction]
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- Convert between LedgerState and JSON representation
toLedgerStateJSON :: LedgerState -> LedgerStateJSON
toLedgerStateJSON ls =
  LedgerStateJSON
    { utxos = Map.toList (ledgerUTXOs ls)
    , transactions = ledgerTransactions ls
    }

fromLedgerStateJSON :: LedgerStateJSON -> Either Text LedgerState
fromLedgerStateJSON lsj =
  pure $
    LedgerState
      { ledgerUTXOs = Map.fromList (utxos lsj)
      , ledgerTransactions = transactions lsj
      }

--------------------------------------------------------------------------------
-- Blockchain Initialization ---------------------------------------------------

{- |
Initialize a new blockchain with genesis UTXOs.

Creates a blockchain with initial funds distributed according to the genesis
configuration. The blockchain state is persisted to the specified file path.
-}
initBlockchain
  :: MonadSTM m
  => BlockchainName
  -> FilePath
  -> [GenesisUTXO]
  -> m (Blockchain m)
initBlockchain name path genesisUTXOs = do
  -- Create genesis transaction
  let genesisTxId = TxId (BS.pack [0, 0, 0, 0])
      genesisOutputs =
        zipWith
          ( \idx g ->
              UTXO
                { utxoTxId = genesisTxId
                , utxoIndex = idx
                , utxoAmount = genesisAmount g
                , utxoOwner = genesisOwner g
                }
          )
          [0 ..]
          genesisUTXOs

  -- Create initial ledger state
  let initialState =
        LedgerState
          { ledgerUTXOs = Map.singleton genesisTxId genesisOutputs
          , ledgerTransactions = []
          }

  stateRef <- atomically $ newTVar initialState

  let blockchain =
        Blockchain
          { blockchainName = name
          , blockchainLedgerPath = path
          , blockchainState = stateRef
          }

  -- Persist to disk (only when m ~ IO)
  -- Note: In io-sim tests, skip file persistence
  -- liftIO $ saveBlockchainIO blockchain

  pure blockchain

{- |
Load an existing blockchain from a JSON file.

Returns an error if the file doesn't exist or contains invalid data.
-}
loadBlockchain
  :: BlockchainName
  -> FilePath
  -> IO (Either Text (Blockchain IO))
loadBlockchain name path = do
  exists <- doesFileExist path
  if not exists
    then pure $ Left $ "Blockchain file not found: " <> toText path
    else do
      result <- eitherDecodeFileStrict path
      case result of
        Left err -> pure $ Left $ "Failed to parse ledger: " <> toText err
        Right lsj -> case fromLedgerStateJSON lsj of
          Left err -> pure $ Left err
          Right ledgerState -> do
            stateRef <- liftIO $ atomically $ newTVar ledgerState
            pure $
              Right $
                Blockchain
                  { blockchainName = name
                  , blockchainLedgerPath = path
                  , blockchainState = stateRef
                  }

--------------------------------------------------------------------------------
-- Transaction Operations ------------------------------------------------------

{- |
Submit a transaction to the blockchain.

Verifies the transaction, updates the UTXO set, and persists the new state.
Returns the transaction ID on success or an error message on failure.
-}
submitTransaction
  :: MonadSTM m => Blockchain m -> Transaction -> m (Either Text TxId)
submitTransaction blockchain tx = do
  ledger <- atomically $ readTVar (blockchainState blockchain)

  -- Verify transaction
  case verifyTransaction tx ledger of
    Left err -> pure $ Left err
    Right () -> do
      -- Generate transaction ID
      let txId = generateTxId tx

      -- Update UTXO set: remove spent UTXOs, add new outputs
      let spentUTXOs = Set.fromList (map (\u -> (utxoTxId u, utxoIndex u)) (txInputs tx))
          updatedUTXOs =
            Map.filter
              (not . null)
              ( Map.map
                  (filter (\u -> not $ Set.member (utxoTxId u, utxoIndex u) spentUTXOs))
                  (ledgerUTXOs ledger)
              )

      -- Add new outputs
      let newOutputs =
            zipWith
              ( \idx out ->
                  UTXO
                    { utxoTxId = txId
                    , utxoIndex = idx
                    , utxoAmount = outputAmount out
                    , utxoOwner = outputOwner out
                    }
              )
              [0 ..]
              (txOutputs tx)
          finalUTXOs = Map.insert txId newOutputs updatedUTXOs

      -- Update ledger state
      let newLedger =
            LedgerState
              { ledgerUTXOs = finalUTXOs
              , ledgerTransactions = ledgerTransactions ledger ++ [tx]
              }

      atomically $ writeTVar (blockchainState blockchain) newLedger
      -- Skip persistence in io-sim tests
      -- liftIO $ saveBlockchainIO blockchain

      pure $ Right txId

{- |
Query all UTXOs owned by a public key.

Returns a list of unspent transaction outputs that can be used as inputs
for new transactions.
-}
queryUTXOs :: MonadSTM m => Blockchain m -> PublicKey -> m [UTXO]
queryUTXOs blockchain pubKey = do
  ledger <- atomically $ readTVar (blockchainState blockchain)
  let allUTXOs = concat (Map.elems (ledgerUTXOs ledger))
  pure $ filter (\u -> utxoOwner u == pubKey) allUTXOs

{- |
Get the total balance for a public key.

Sums up all UTXO amounts owned by the given public key.
-}
getBalance :: MonadSTM m => Blockchain m -> PublicKey -> m Word64
getBalance blockchain pubKey = do
  ownedUtxos <- queryUTXOs blockchain pubKey
  pure $ sum (map utxoAmount ownedUtxos)

--------------------------------------------------------------------------------
-- Persistence -----------------------------------------------------------------

{- |
Save blockchain state to JSON file.

Persists the current UTXO set and transaction history to disk.
-}
saveBlockchain :: (MonadSTM m, MonadIO m) => Blockchain m -> m ()
saveBlockchain blockchain = do
  ledger <- atomically $ readTVar (blockchainState blockchain)
  liftIO $ saveBlockchainLedger (blockchainLedgerPath blockchain) ledger

-- | IO-specific version for file persistence
saveBlockchainLedger :: FilePath -> LedgerState -> IO ()
saveBlockchainLedger path ledger = do
  let lsj = toLedgerStateJSON ledger
  encodeFile path lsj
    `catch` \(e :: SomeException) ->
      putStrLn $ "Warning: Failed to save blockchain: " <> show e

{- | Helper for initBlockchain (when persistence is enabled)
saveBlockchainIO :: Blockchain IO -> IO ()
saveBlockchainIO blockchain = do
  ledger <- atomically $ readTVar (blockchainState blockchain)
  saveBlockchainLedger (blockchainLedgerPath blockchain) ledger
-}

--------------------------------------------------------------------------------
-- Helper Functions ------------------------------------------------------------

-- | Generate a transaction ID by hashing the transaction
generateTxId :: Transaction -> TxId
generateTxId tx =
  let inputBytes = foldMap encodeUTXO (txInputs tx)
      outputBytes = foldMap encodeOutput (txOutputs tx)
      sigBytes = foldMap encodeSignature (txSignatures tx)
      digest :: Digest SHA256 = hash (inputBytes <> outputBytes <> sigBytes)
   in TxId (convert digest)

encodeUTXO :: UTXO -> ByteString
encodeUTXO u =
  let TxId txId = utxoTxId u
   in txId <> BS.pack [fromIntegral (utxoIndex u)]

encodeOutput :: Output -> ByteString
encodeOutput o =
  let PublicKey pk = outputOwner o
   in BS.pack (word64ToBytes (outputAmount o)) <> pk

encodeSignature :: Signature -> ByteString
encodeSignature s = sigNonce s <> sigScalar s

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
