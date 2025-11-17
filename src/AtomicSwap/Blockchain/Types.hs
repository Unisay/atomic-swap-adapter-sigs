{- |
Module: AtomicSwap.Blockchain.Types
Description: Blockchain-specific type definitions

This module defines types for simulating two independent blockchains (ChainA
and ChainB) using file-based ledgers. Each blockchain maintains a UTXO set
and transaction history.

Uses TVar from io-classes for testability with io-sim.
-}
module AtomicSwap.Blockchain.Types
  ( -- * Blockchain State
    Blockchain (..)
  , BlockchainName
  , LedgerState (..)

    -- * Genesis Configuration
  , GenesisUTXO (..)
  ) where

import Prelude hiding (STM, TVar, atomically, newTVar, readTVar, writeTVar)

import Control.Concurrent.Class.MonadSTM.TVar (TVar)

import AtomicSwap.Types (PublicKey, Transaction, TxId, UTXO)

--------------------------------------------------------------------------------
-- Blockchain State ------------------------------------------------------------

-- | Name identifier for a blockchain (e.g., "ChainA", "ChainB")
type BlockchainName = Text

{- | A simulated blockchain with file-based persistence

Polymorphic over the monad type to allow use with both IO and IOSim.
-}
data Blockchain m = Blockchain
  { blockchainName :: BlockchainName
  , blockchainLedgerPath :: FilePath
  , blockchainState :: TVar m LedgerState
  }

-- | The current state of the blockchain ledger
data LedgerState = LedgerState
  { ledgerUTXOs :: Map TxId [UTXO]
  -- ^ Current UTXO set indexed by transaction ID
  , ledgerTransactions :: [Transaction]
  -- ^ All transactions in chronological order
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Genesis Configuration -------------------------------------------------------

-- | Genesis UTXO for initializing blockchain with funds
data GenesisUTXO = GenesisUTXO
  { genesisOwner :: PublicKey
  , genesisAmount :: Word64
  }
  deriving stock (Show, Eq)
