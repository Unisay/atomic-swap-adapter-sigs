{- |
Module: AtomicSwap.Types
Description: Core types for atomic swap protocol

This module defines the fundamental types used throughout the atomic swap
implementation, including parties, transactions, UTXOs, and protocol state.
-}
module AtomicSwap.Types
  ( -- * Party Types
    Party (..)
  , Participant (..)

    -- * Cryptographic Types
  , PrivateKey (..)
  , PublicKey (..)
  , Ed25519PrivateKey (..)
  , Signature (..)
  , AdaptedSignature (..)
  , AdapterSecret (..)
  , AdapterPoint (..)
  , NIZKProof (..)

    -- * Transaction Types
  , Transaction (..)
  , TxId (..)
  , UTXO (..)
  , Output (..)

    -- * Protocol Message Types
  , Message (..)

    -- * Protocol State
  , SwapState (..)
  , SwapResult (..)
  ) where

import Data.ByteString.Base16 qualified as Base16
import Data.Text.Encoding qualified as TE
import GHC.Show qualified
import NoThunks.Class (NoThunks (..))

--------------------------------------------------------------------------------
-- Helper for Hex-Encoded Show Instances ---------------------------------------

{- |
Newtype wrapper for ByteString with hexadecimal Show instance.

This is used via DerivingVia to automatically provide hex-encoded
Show instances for all ByteString-based newtypes.
-}
newtype HexBytes = HexBytes ByteString
  deriving newtype (Eq, Ord)

instance GHC.Show.Show HexBytes where
  show (HexBytes bytes) = toString (TE.decodeUtf8 (Base16.encode bytes))

--------------------------------------------------------------------------------
-- Party Types -----------------------------------------------------------------

-- | Protocol participant identifier (simple enum)
data Participant = Alice | Bob
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass NFData

instance NoThunks Participant

-- | A party participating in the atomic swap protocol (full state)
data Party = Party
  { partyParticipant :: Participant
  , partyPrivateKey :: Ed25519PrivateKey
  , partyPublicKey :: PublicKey
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Cryptographic Types ---------------------------------------------------------

-- | Private key scalar (generic)
newtype PrivateKey = PrivateKey ByteString
  deriving stock Eq
  deriving Show via HexBytes

-- | Ed25519 private key for rEdDSA (split into two components)
data Ed25519PrivateKey = Ed25519PrivateKey
  { ed25519SK0 :: PrivateKey -- Signing scalar (clamped)
  , ed25519SK1 :: ByteString -- Nonce generation seed
  }
  deriving stock (Show, Eq)

-- | Public key (elliptic curve point)
newtype PublicKey = PublicKey ByteString
  deriving stock (Eq, Generic)
  deriving Show via HexBytes

-- | Standard signature (R, s)
data Signature = Signature
  { sigNonce :: ByteString -- R point
  , sigScalar :: ByteString -- s scalar
  }
  deriving stock (Show, Eq, Generic)

-- | Adapted signature (R̂, ŝ) - requires adapter secret to complete
data AdaptedSignature = AdaptedSignature
  { adaptedNonce :: ByteString -- R̂ = R + T
  , adaptedScalar :: ByteString -- ŝ (without t)
  }
  deriving stock (Show, Eq)

-- | Adapter secret (scalar y)
newtype AdapterSecret = AdapterSecret ByteString
  deriving stock Eq
  deriving Show via HexBytes

-- | Adapter point (Y = y·B)
newtype AdapterPoint = AdapterPoint ByteString
  deriving stock Eq
  deriving Show via HexBytes

-- | NIZK proof for discrete logarithm relation
newtype NIZKProof = NIZKProof ByteString
  deriving stock (Eq, Generic)
  deriving anyclass (NFData, NoThunks)
  deriving Show via HexBytes

--------------------------------------------------------------------------------
-- Transaction Types -----------------------------------------------------------

-- | Transaction identifier
newtype TxId = TxId ByteString
  deriving stock (Eq, Ord, Generic)
  deriving Show via HexBytes

-- | Unspent Transaction Output
data UTXO = UTXO
  { utxoTxId :: TxId
  , utxoIndex :: Word32
  , utxoAmount :: Word64
  , utxoOwner :: PublicKey
  }
  deriving stock (Show, Eq, Generic)

-- | Transaction output
data Output = Output
  { outputOwner :: PublicKey
  , outputAmount :: Word64
  }
  deriving stock (Show, Eq, Generic)

-- | Transaction
data Transaction = Transaction
  { txInputs :: [UTXO]
  , txOutputs :: [Output]
  , txSignatures :: [Signature]
  }
  deriving stock (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- Protocol Message Types ------------------------------------------------------

-- | Messages exchanged between Alice and Bob
data Message
  = PublicKeyMsg PublicKey
  | AdapterPointMsg AdapterPoint
  | TransactionProposalMsg Transaction AdaptedSignature
  | -- | Alice's complete signature for extraction
    CompleteSignatureMsg Signature
  | SwapCompleteMsg
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Protocol State --------------------------------------------------------------

-- | Current state of the atomic swap protocol
data SwapState
  = StateInitial
  | StateKeysExchanged
  | StateTransactionsCreated
  | StateSignaturesExchanged
  | StateExecuting
  | StateComplete
  deriving stock (Show, Eq, Enum)

-- | Result of atomic swap execution
data SwapResult
  = SwapSuccess
  | SwapFailure Text
  deriving stock (Show, Eq)
