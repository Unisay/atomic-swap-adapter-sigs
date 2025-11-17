{-# LANGUAGE StrictData #-}

{- |
Module: AtomicSwap.Simulator.Class
Description: Abstraction for simulator operations

This module defines the MonadSimulator class which provides a clean
interface for step execution handlers, abstracting over state management
and cryptographic operations.
-}
module AtomicSwap.Simulator.Class
  ( MonadSimulator (..)
  ) where

import AtomicSwap.Prelude
import AtomicSwap.Simulator.State (PartyState)
import AtomicSwap.Simulator.Types
  ( Asset (..)
  , Quantity
  , StateUpdate
  , UserInputs
  )

--------------------------------------------------------------------------------
-- MonadSimulator Class --------------------------------------------------------

-- | Abstraction for simulator operations used by step execution handlers
class Monad m => MonadSimulator m where
  -- | Get current step count (after all updates applied)
  getStepCount :: m Int

  -- | Get complete party state
  getPartyState :: Participant -> m PartyState

  -- | Get agreed swap amounts
  getSwapAmounts :: m (Quantity 'Apple, Quantity 'Banana)

  -- | Apply state updates through the append-only log
  applyUpdates :: Participant -> UserInputs -> [StateUpdate] -> m ()

  -- | Generate Ed25519 keypair (delegated to crypto backend)
  generateKeyPair :: m (Ed25519PrivateKey, PublicKey)

  -- | Generate adapter secret (random scalar)
  generateAdapterSecret :: m AdapterSecret

  -- | Generate adapter commitment from secret (Y = y·B)
  generateAdapterCommitment :: AdapterSecret -> m AdapterPoint

  -- | Generate NIZK proof for adapter commitment
  generateNIZKProof :: AdapterSecret -> AdapterPoint -> m NIZKProof

  -- | Verify NIZK proof for adapter commitment
  verifyNIZKProof :: AdapterPoint -> NIZKProof -> m Bool

  -- | Build a dummy transaction for simulator (represents the asset transfer)
  buildDummyTransaction
    :: PublicKey
    -- ^ Recipient's public key
    -> Natural
    -- ^ Amount to transfer
    -> m Transaction

  -- | Create adapter pre-signature for a transaction
  createPreSignature
    :: Ed25519PrivateKey
    -> PublicKey
    -> Transaction
    -> AdapterPoint
    -> NIZKProof
    -> m AdaptedSignature

  -- | Verify adapter pre-signature
  verifyPreSignature
    :: PublicKey
    -- ^ Signer's public key
    -> Transaction
    -- ^ Transaction being signed
    -> AdapterPoint
    -- ^ Adapter commitment point
    -> AdaptedSignature
    -- ^ Pre-signature to verify
    -> NIZKProof
    -- ^ NIZK proof for adapter commitment
    -> m Bool

  -- | Complete signature by adding adapter secret
  completeSignature :: AdaptedSignature -> AdapterSecret -> m Signature

  -- | Extract adapter secret from pre-signature and complete signature
  extractSecret :: AdaptedSignature -> Signature -> m AdapterSecret
