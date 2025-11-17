{-# LANGUAGE StrictData #-}

{- |
Module: AtomicSwap.Simulator.Steps
Description: Step execution handlers for atomic swap simulator

This module provides pure step execution logic separated from HTTP concerns.
Each executor takes inputs via MonadSimulator operations and returns view data.

= State Management Pattern

All step executors follow the Event Sourcing pattern:

1. Read current state via 'getPartyState' (read-only projection)
2. Generate new cryptographic values
3. Call 'applyUpdates' to append StateUpdates to GlobalState
4. Read updated state projection via 'getPartyState'
5. Return 'RenderContext' with the acting participant's updated state

The 'applyUpdates' participant parameter indicates WHO performed the action,
while the StateUpdate list can contain changes for ANY participant(s).
Both party states are automatically recomputed from the append-only log.
-}
module AtomicSwap.Simulator.Steps
  ( -- * Step Result
    StepResult (..)
  , ok
  , failed

    -- * Step Executors
  , executeAliceKeygen
  , executeBobKeygen
  , executeAliceGenerateSecret
  , executeAliceMakeCommitment
  , executeAliceSendPublicKey
  , executeBobSendPublicKey
  , executeAliceSendCommitment
  , executeAliceGenerateNIZKProof
  , executeAliceSendNIZKProof
  , executeBobVerifyNIZKProof
  , executeAlicePrepareTransaction
  , executeAliceCreatePreSignature
  , executeAlicePublishPreSignature
  ) where

import Prelude

import AtomicSwap.Simulator.Class (MonadSimulator (..))
import AtomicSwap.Simulator.State (PartyState (..))
import AtomicSwap.Simulator.Types
  ( Participant (..)
  , Quantity (..)
  , StateUpdate (..)
  , UserInputs (..)
  )
import Data.Strict.Maybe qualified as SM

--------------------------------------------------------------------------------
-- Step Result Type ------------------------------------------------------------

-- | Result of executing a step handler
data StepResult
  = StepOk -- Precondition met, step executed
  | StepFailed Text -- Precondition not met, error message
  deriving stock (Show, Eq)

-- | Smart constructor for success
ok :: Monad m => m StepResult
ok = pure StepOk

-- | Smart constructor for failure
failed :: Monad m => Text -> m StepResult
failed = pure . StepFailed

--------------------------------------------------------------------------------
-- Step Executors --------------------------------------------------------------

-- | Execute Alice's keypair generation step
executeAliceKeygen :: MonadSimulator m => m StepResult
executeAliceKeygen = do
  (sk, pk) <- generateKeyPair
  applyUpdates
    Alice
    (UserInputs "")
    [SetPrivateKey Alice sk, SetPublicKey Alice pk]
  ok

-- | Execute Bob's keypair generation step
executeBobKeygen :: MonadSimulator m => m StepResult
executeBobKeygen = do
  (sk, pk) <- generateKeyPair
  applyUpdates Bob (UserInputs "") [SetPrivateKey Bob sk, SetPublicKey Bob pk]
  ok

-- | Execute Alice's adapter secret generation step
executeAliceGenerateSecret :: MonadSimulator m => m StepResult
executeAliceGenerateSecret = do
  secret <- generateAdapterSecret
  applyUpdates Alice (UserInputs "") [SetAdapterSecret Alice secret]
  ok

-- | Execute Alice's commitment generation step
executeAliceMakeCommitment :: MonadSimulator m => m StepResult
executeAliceMakeCommitment = do
  aliceState <- getPartyState Alice
  case psAdapterSecret aliceState of
    SM.Nothing -> failed "adapter secret not set"
    SM.Just secret -> do
      commitment <- generateAdapterCommitment secret
      applyUpdates Alice (UserInputs "") [SetAdapterCommitment Alice commitment]
      ok

-- | Execute Alice sending her public key to Bob
executeAliceSendPublicKey :: MonadSimulator m => m StepResult
executeAliceSendPublicKey = do
  -- 1. Read current state (read-only projection)
  aliceState <- getPartyState Alice
  case psPublicKey aliceState of
    SM.Nothing -> failed "Alice has no public key"
    SM.Just alicePk -> do
      -- 2. Append updates to GlobalState (Alice is the actor)
      --    Updates affect BOTH participants (Bob receives, Alice marks sent)
      --    State-diffing will automatically detect both parties changed
      applyUpdates
        Alice -- Actor who initiated this step
        (UserInputs "")
        [ SetOtherPartyPublicKey Bob alicePk -- Bob's state changes
        , SetSentPublicKey Alice True -- Alice's state changes
        ]
      ok

-- | Execute Bob sending his public key to Alice
executeBobSendPublicKey :: MonadSimulator m => m StepResult
executeBobSendPublicKey = do
  bobState <- getPartyState Bob
  case psPublicKey bobState of
    SM.Nothing -> failed "Bob has no public key"
    SM.Just bobPk -> do
      -- Alice receives Bob's public key, Bob marks as sent (cross-participant update)
      -- State-diffing will automatically detect both parties changed
      applyUpdates
        Bob
        (UserInputs "")
        [ SetOtherPartyPublicKey Alice bobPk
        , SetSentPublicKey Bob True
        ]
      ok

-- | Execute Alice's NIZK proof generation step
executeAliceGenerateNIZKProof :: MonadSimulator m => m StepResult
executeAliceGenerateNIZKProof = do
  aliceState <- getPartyState Alice
  case (psAdapterSecret aliceState, psAdapterCommitment aliceState) of
    (SM.Nothing, _) -> failed "adapter secret not set"
    (_, SM.Nothing) -> failed "adapter commitment not set"
    (SM.Just secret, SM.Just commitment) -> do
      proof <- generateNIZKProof secret commitment
      applyUpdates Alice (UserInputs "") [SetNIZKProof Alice proof]
      ok

-- | Execute Alice sending commitment to Bob
executeAliceSendCommitment :: MonadSimulator m => m StepResult
executeAliceSendCommitment = do
  aliceState <- getPartyState Alice
  case psAdapterCommitment aliceState of
    SM.Nothing -> failed "Adapter commitment not generated"
    SM.Just commitment -> do
      -- Bob receives commitment, Alice marks as sent (cross-participant update)
      applyUpdates
        Alice
        (UserInputs "")
        [ SetOtherPartyCommitment Bob commitment
        , SetSentCommitment Alice True
        ]
      ok

-- | Execute Alice sending NIZK proof to Bob
executeAliceSendNIZKProof :: MonadSimulator m => m StepResult
executeAliceSendNIZKProof = do
  aliceState <- getPartyState Alice
  case psNIZKProof aliceState of
    SM.Nothing -> failed "NIZK proof not generated"
    SM.Just proof -> do
      -- Bob receives NIZK proof, Alice marks as sent (cross-participant update)
      applyUpdates
        Alice
        (UserInputs "")
        [ SetOtherPartyNIZKProof Bob proof
        , SetSentNIZKProof Alice True
        ]
      ok

-- | Execute Bob's NIZK proof verification
executeBobVerifyNIZKProof :: MonadSimulator m => m StepResult
executeBobVerifyNIZKProof = do
  bobState <- getPartyState Bob
  case (psOtherPartyCommitment bobState, psOtherPartyNIZKProof bobState) of
    (SM.Nothing, _) -> failed "Alice's commitment not received"
    (_, SM.Nothing) -> failed "Alice's NIZK proof not received"
    (SM.Just commitment, SM.Just proof) -> do
      isValid <- verifyNIZKProof commitment proof
      if isValid
        then do
          applyUpdates Bob (UserInputs "") [SetNIZKProofVerified Bob True]
          ok
        else failed "NIZK proof verification failed"

--------------------------------------------------------------------------------
-- Phase 2: Pre-Signature Creation ---------------------------------------------

-- | Execute Alice's transaction preparation (Step 1 of pre-signature phase)
executeAlicePrepareTransaction :: MonadSimulator m => m StepResult
executeAlicePrepareTransaction = do
  aliceState <- getPartyState Alice
  case (psOtherPartyPublicKey aliceState, psSentNIZKProof aliceState) of
    (SM.Nothing, _) -> failed "Bob's public key not received"
    (_, False) -> failed "NIZK proof not sent to Bob yet"
    (SM.Just bobPk, True) -> do
      -- Get agreed swap amounts
      (Quantity apples, _) <- getSwapAmounts
      -- Build transaction sending apples to Bob
      tx <- buildDummyTransaction bobPk apples
      applyUpdates Alice (UserInputs "") [SetTransaction Alice tx]
      ok

-- | Execute Alice's adapter pre-signature creation (Step 2)
executeAliceCreatePreSignature :: MonadSimulator m => m StepResult
executeAliceCreatePreSignature = do
  aliceState <- getPartyState Alice
  case ( psTransaction aliceState
       , psPrivateKey aliceState
       , psPublicKey aliceState
       , psAdapterCommitment aliceState
       , psNIZKProof aliceState
       ) of
    (SM.Nothing, _, _, _, _) -> failed "Transaction not prepared"
    (_, SM.Nothing, _, _, _) -> failed "Private key not set"
    (_, _, SM.Nothing, _, _) -> failed "Public key not set"
    (_, _, _, SM.Nothing, _) -> failed "Adapter commitment not set"
    (_, _, _, _, SM.Nothing) -> failed "NIZK proof not set"
    (SM.Just tx, SM.Just privKey, SM.Just pubKey, SM.Just commitment, SM.Just proof) ->
      do
        preSig <- createPreSignature privKey pubKey tx commitment proof
        applyUpdates Alice (UserInputs "") [SetPreSignature Alice preSig]
        ok

-- | Execute Alice publishing pre-signature to Bob (Step 3)
executeAlicePublishPreSignature :: MonadSimulator m => m StepResult
executeAlicePublishPreSignature = do
  aliceState <- getPartyState Alice
  case (psTransaction aliceState, psPreSignature aliceState) of
    (SM.Nothing, _) -> failed "Transaction not prepared"
    (_, SM.Nothing) -> failed "Pre-signature not created"
    (SM.Just tx, SM.Just preSig) -> do
      -- Bob receives both transaction and pre-signature (cross-participant update)
      applyUpdates
        Alice
        (UserInputs "")
        [ SetSentPreSignature Alice True
        , SetOtherPartyTransaction Bob tx
        , SetOtherPartyPreSignature Bob preSig
        ]
      ok
