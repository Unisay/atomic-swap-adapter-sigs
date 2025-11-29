{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}

module AtomicSwap.Simulator.Types
  ( -- * Core Types
    Action (..)
  , Participant (..)
  , Asset (..)
  , Quantity (..)
  , Chain (..)
  , SomeChain (..)

    -- * State Management
  , StepIndex (..)
  , StateUpdate (..)
  , GlobalState
  , StepEntry

    -- * Step Execution
  , HandlerId (..)
  , UserInputs (..)
  , StepResult (..)
  , UIMarkup (..)
  ) where

import AtomicSwap.Prelude hiding (Party)
import NoThunks.Class (NoThunks (..))

--------------------------------------------------------------------------------
-- Core Enums ------------------------------------------------------------------

-- | User-facing actions in the simulator
data Action
  = AliceKeygen
  | BobKeygen
  | AliceSendPublicKey
  | BobSendPublicKey
  | AliceGenerateSecret
  | AliceMakeCommitment
  | AliceSendCommitment
  | AliceGenerateNIZKProof
  | AliceSendNIZKProof
  | BobVerifyNIZKProof
  | AlicePrepareTransaction
  | AliceCreatePreSignature
  | AlicePublishPreSignature
  | BobVerifyAlicePreSignature
  | BobPrepareTransaction
  | BobCreatePreSignature
  | BobPublishPreSignature
  | AliceVerifyBobPreSignature
  | AliceCompleteSignature
  | BobExtractSecret
  | BobCompleteSignature
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, NoThunks)

-- | Asset types (type-level)
data Asset = Apple | Banana
  deriving stock Generic

instance NoThunks Asset

-- | Type-safe quantity indexed by asset type
newtype Quantity (asset :: Asset) = Quantity Natural
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype Num
  deriving anyclass NFData

-- Manual NoThunks for indexed newtype
instance NoThunks (Quantity asset) where
  wNoThunks _ctx _ = return Nothing
  showTypeOf _proxy = "Quantity"

--------------------------------------------------------------------------------
-- Chain GADT ------------------------------------------------------------------

-- | Blockchain indexed by its native asset
data Chain (asset :: Asset) where
  AppleChain :: Chain 'Apple
  BananaChain :: Chain 'Banana

deriving stock instance Eq (Chain asset)
deriving stock instance Ord (Chain asset)
deriving stock instance Show (Chain asset)

-- Manual NoThunks for GADT
instance NoThunks (Chain asset) where
  wNoThunks _ctx _ = return Nothing
  showTypeOf _proxy = "Chain"

-- | Existential wrapper for runtime storage
data SomeChain where
  SomeChain :: Chain asset -> SomeChain

deriving stock instance Show SomeChain

instance Eq SomeChain where
  SomeChain AppleChain == SomeChain AppleChain = True
  SomeChain BananaChain == SomeChain BananaChain = True
  _ == _ = False

instance Ord SomeChain where
  compare (SomeChain AppleChain) (SomeChain AppleChain) = EQ
  compare (SomeChain AppleChain) (SomeChain BananaChain) = LT
  compare (SomeChain BananaChain) (SomeChain AppleChain) = GT
  compare (SomeChain BananaChain) (SomeChain BananaChain) = EQ

instance NoThunks SomeChain where
  wNoThunks _ctx _ = return Nothing
  showTypeOf _proxy = "SomeChain"

--------------------------------------------------------------------------------
-- State Updates ---------------------------------------------------------------

-- | Index for steps in execution history (0-based)
newtype StepIndex = StepIndex Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, Enum)
  deriving anyclass NFData

instance NoThunks StepIndex

-- | Fine-grained state mutations
data StateUpdate
  = -- Cryptographic state
    SetPrivateKey Participant Ed25519PrivateKey
  | SetPublicKey Participant PublicKey
  | SetOtherPartyPublicKey Participant PublicKey
  | SetSentPublicKey Participant Bool
  | SetAdapterSecret Participant AdapterSecret
  | SetAdapterCommitment Participant AdapterPoint
  | SetSentCommitment Participant Bool
  | SetOtherPartyCommitment Participant AdapterPoint
  | SetNIZKProof Participant NIZKProof
  | SetSentNIZKProof Participant Bool
  | SetOtherPartyNIZKProof Participant NIZKProof
  | SetNIZKProofVerified Participant Bool
  | SetTransaction Participant Transaction
  | SetPreSignature Participant AdaptedSignature
  | SetSentPreSignature Participant Bool
  | SetOtherPartyTransaction Participant Transaction
  | SetOtherPartyPreSignature Participant AdaptedSignature
  | SetPreSignatureVerified Participant Bool
  | SetCompleteSignature Participant Signature
  | SetOtherPartyCompleteSignature Participant Signature
  | SetExtractedSecret Participant AdapterSecret
  | -- Blockchain state
    PostTransaction SomeChain TxId Transaction
  | ObserveTransaction Participant SomeChain TxId
  | UpdateUTXO SomeChain UTXO
  | -- Message passing
    SendMessage Participant Text -- Message type TBD
  | ReceiveMessage Participant Text
  | -- Thread state
    SetThreadWaiting Participant Bool
  deriving stock (Eq, Show, Generic)

-- Manual instances since it contains external types
instance NoThunks StateUpdate where
  wNoThunks _ctx _ = return Nothing
  showTypeOf _proxy = "StateUpdate"

instance NFData StateUpdate where rnf _ = ()

--------------------------------------------------------------------------------
-- Global State ----------------------------------------------------------------

-- | Single entry in execution history
type StepEntry = (StepIndex, Participant, UserInputs, [StateUpdate])

-- | Complete execution history (append-only log)
type GlobalState = [StepEntry]

--------------------------------------------------------------------------------
-- Step Execution --------------------------------------------------------------

-- | Identifier for step handler functions
newtype HandlerId = HandlerId Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype IsString
  deriving anyclass NFData

instance NoThunks HandlerId

-- | User-provided inputs for a step (editable in UI)
newtype UserInputs = UserInputs Text -- Placeholder, will expand
  deriving stock (Eq, Show, Generic)
  deriving newtype IsString
  deriving anyclass NFData

instance NoThunks UserInputs

-- | Result of executing a step handler
data StepResult = StepResult
  { stateUpdates :: [StateUpdate]
  , nextUI :: UIMarkup
  , nextHandler :: HandlerId
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass NFData

instance NoThunks StepResult

-- | UI markup for rendering a step
data UIMarkup = UIMarkup
  { stepLabel :: Text
  , actorLabel :: Text
  , partyStateTable :: [(Text, Text)] -- (Name, Value) pairs
  , inputFields :: [(Text, Text)] -- (FieldName, DefaultValue) pairs
  , nextStepButton :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass NFData

instance NoThunks UIMarkup
