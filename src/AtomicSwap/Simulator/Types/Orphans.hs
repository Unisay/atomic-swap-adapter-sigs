{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module: AtomicSwap.Simulator.Types.Orphans
Description: NoThunks orphan instances

Orphan instances for NoThunks on types from AtomicSwap.Types.
These are orphans because we don't want to add nothunks dependency
to the core Types module.
-}
module AtomicSwap.Simulator.Types.Orphans () where

import AtomicSwap.Types
  ( AdapterPoint
  , AdapterSecret
  , Ed25519PrivateKey
  , PublicKey
  , Signature
  , Transaction
  , TxId
  , UTXO
  )
import Data.Strict.Maybe qualified as SM
import Data.Strict.Sequence qualified as Seq
import NoThunks.Class (NoThunks (..))

-- Orphan instances for AtomicSwap.Types (manual - they contain ByteString)
instance NoThunks Ed25519PrivateKey where
  wNoThunks _ctx _ = return Nothing
  showTypeOf _proxy = "Ed25519PrivateKey"

instance NoThunks PublicKey where
  wNoThunks _ctx _ = return Nothing
  showTypeOf _proxy = "PublicKey"

instance NoThunks Signature where
  wNoThunks _ctx _ = return Nothing
  showTypeOf _proxy = "Signature"

instance NoThunks TxId where
  wNoThunks _ctx _ = return Nothing
  showTypeOf _proxy = "TxId"

instance NoThunks Transaction where
  wNoThunks _ctx _ = return Nothing
  showTypeOf _proxy = "Transaction"

instance NoThunks UTXO where
  wNoThunks _ctx _ = return Nothing
  showTypeOf _proxy = "UTXO"

instance NoThunks AdapterSecret where
  wNoThunks _ctx _ = return Nothing
  showTypeOf _proxy = "AdapterSecret"

instance NoThunks AdapterPoint where
  wNoThunks _ctx _ = return Nothing
  showTypeOf _proxy = "AdapterPoint"

-- NFData instances for AtomicSwap.Types
instance NFData Ed25519PrivateKey where rnf _ = ()
instance NFData PublicKey where rnf _ = ()
instance NFData Signature where rnf _ = ()
instance NFData TxId where rnf _ = ()
instance NFData Transaction where rnf _ = ()
instance NFData UTXO where rnf _ = ()
instance NFData AdapterSecret where rnf _ = ()
instance NFData AdapterPoint where rnf _ = ()

-- NoThunks instances for strict types
instance NoThunks a => NoThunks (SM.Maybe a) where
  wNoThunks _ctx SM.Nothing = return Nothing
  wNoThunks ctx (SM.Just x) = wNoThunks ctx x
  showTypeOf _proxy = "Strict.Maybe"

instance NoThunks a => NoThunks (Seq.Seq a) where
  wNoThunks _ctx _ = return Nothing
  showTypeOf _proxy = "Strict.Seq"
