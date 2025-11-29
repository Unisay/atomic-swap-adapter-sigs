{-# LANGUAGE StrictData #-}

{- |
Module: AtomicSwap.Simulator.Render.State
Description: State panel rendering for both participants

Provides rendering functions for Alice's and Bob's state panels including
keys, secrets, transactions, signatures, and OOB update wrappers for HTMX.
-}
module AtomicSwap.Simulator.Render.State
  ( -- * State Field Rendering
    renderAliceStateFields
  , renderBobStateFields

    -- * OOB Update Wrappers
  , renderAliceStateUpdate
  , renderBobStateUpdate
  , renderAliceBlockchainUpdate
  , renderBobBlockchainUpdate

    -- * Blockchain Visualization
  , renderAliceBlockchain
  , renderBobBlockchain

    -- * State Rendering with Amounts
  , renderAliceState
  , renderBobState
  ) where

import Data.Strict.Maybe qualified as SM
import Lucid
  ( Html
  , class_
  , div_
  , span_
  , toHtml
  )
import Prelude

import AtomicSwap.Simulator.Render.Helpers
  ( formatHex
  , hxSwapOob_
  , section_
  , stateValueInput_
  )
import AtomicSwap.Simulator.State (PartyState (..))
import AtomicSwap.Simulator.Types (Asset (..), Quantity (..))
import AtomicSwap.Types
  ( AdaptedSignature (..)
  , AdapterPoint (..)
  , AdapterSecret (..)
  , Ed25519PrivateKey (..)
  , NIZKProof (..)
  , PrivateKey (..)
  , PublicKey (..)
  , Signature (..)
  , Transaction (..)
  )

-- | Render Alice's state fields (no OOB wrapper)
renderAliceStateFields :: PartyState -> Html ()
renderAliceStateFields partyState = do
  let stateClass isFresh =
        if isFresh
          then "state-item state-set recently-updated"
          else "state-item state-set"
  -- Private Key (sk0, sk1)
  case SM.maybe Nothing Just (psPrivateKey partyState) of
    Just (Ed25519PrivateKey (PrivateKey sk0) sk1) -> do
      div_ [class_ (stateClass (psPrivateKeyFresh partyState))] do
        span_ [class_ "state-label"] "Private Key (sk0)"
        stateValueInput_ (formatHex sk0)
      div_ [class_ (stateClass (psPrivateKeyFresh partyState))] do
        span_ [class_ "state-label"] "Private Key (sk1)"
        stateValueInput_ (formatHex sk1)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Private Key (sk0)"
        span_ [class_ "state-value"] ""
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Private Key (sk1)"
        span_ [class_ "state-value"] ""
  -- Public Key
  case SM.maybe Nothing Just (psPublicKey partyState) of
    Just (PublicKey pkBytes) -> do
      div_ [class_ (stateClass (psPublicKeyFresh partyState))] do
        span_ [class_ "state-label"] "Public Key"
        stateValueInput_ (formatHex pkBytes)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Public Key"
        span_ [class_ "state-value"] ""
  -- Bob's Public Key (received from Bob)
  case SM.maybe Nothing Just (psOtherPartyPublicKey partyState) of
    Just (PublicKey pkBytes) -> do
      div_ [class_ (stateClass (psOtherPartyPublicKeyFresh partyState))] do
        span_ [class_ "state-label"] "Bob's Public Key"
        stateValueInput_ (formatHex pkBytes)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Bob's Public Key"
        span_ [class_ "state-value"] ""
  -- Adapter Secret
  case SM.maybe Nothing Just (psAdapterSecret partyState) of
    Just (AdapterSecret secretBytes) -> do
      div_ [class_ (stateClass (psAdapterSecretFresh partyState))] do
        span_ [class_ "state-label"] "Adapter Secret (y)"
        stateValueInput_ (formatHex secretBytes)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Adapter Secret (y)"
        span_ [class_ "state-value"] ""
  -- Commitment
  case SM.maybe Nothing Just (psAdapterCommitment partyState) of
    Just (AdapterPoint commitBytes) -> do
      div_ [class_ (stateClass (psAdapterCommitmentFresh partyState))] do
        span_ [class_ "state-label"] "Seal (Y)"
        stateValueInput_ (formatHex commitBytes)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Seal (Y)"
        span_ [class_ "state-value"] ""
  -- Seal Proof
  case SM.maybe Nothing Just (psNIZKProof partyState) of
    Just (NIZKProof proofBytes) -> do
      div_ [class_ (stateClass (psNIZKProofFresh partyState))] do
        span_ [class_ "state-label"] "Seal Proof"
        stateValueInput_ (formatHex proofBytes)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Seal Proof"
        span_ [class_ "state-value"] ""
  -- Transaction
  case SM.maybe Nothing Just (psTransaction partyState) of
    Just tx -> do
      div_ [class_ (stateClass (psTransactionFresh partyState))] do
        span_ [class_ "state-label"] "Transaction"
        span_ [class_ "state-value"] $
          toHtml
            ( show (length (txInputs tx))
                <> " inputs, "
                <> show (length (txOutputs tx))
                <> " outputs"
                :: Text
            )
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Transaction"
        span_ [class_ "state-value"] ""
  -- Pre-Signature
  case SM.maybe Nothing Just (psPreSignature partyState) of
    Just (AdaptedSignature rSign sigTilde) -> do
      div_ [class_ (stateClass (psPreSignatureFresh partyState))] do
        span_ [class_ "state-label"] "Pre-Signature (σ̃)"
        stateValueInput_ (formatHex rSign <> " || " <> formatHex sigTilde)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Pre-Signature (σ̃)"
        span_ [class_ "state-value"] ""
  -- Alice's Complete Signature
  case SM.maybe Nothing Just (psCompleteSignature partyState) of
    Just (Signature rSign sig) -> do
      div_ [class_ (stateClass (psCompleteSignatureFresh partyState))] do
        span_ [class_ "state-label"] "Complete Sig (σᴬ)"
        stateValueInput_ (formatHex rSign <> " || " <> formatHex sig)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Complete Sig (σᴬ)"
        span_ [class_ "state-value"] ""
  -- Bob's Transaction (received from Bob)
  case SM.maybe Nothing Just (psOtherPartyTransaction partyState) of
    Just tx -> do
      div_ [class_ (stateClass (psOtherPartyTransactionFresh partyState))] do
        span_ [class_ "state-label"] "Bob's Transaction"
        span_ [class_ "state-value"] $
          toHtml
            ( show (length (txInputs tx))
                <> " inputs, "
                <> show (length (txOutputs tx))
                <> " outputs"
                :: Text
            )
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Bob's Transaction"
        span_ [class_ "state-value"] ""
  -- Bob's Pre-Signature (received from Bob)
  case SM.maybe Nothing Just (psOtherPartyPreSignature partyState) of
    Just (AdaptedSignature rSign sigTilde) -> do
      div_ [class_ (stateClass (psOtherPartyPreSignatureFresh partyState))] do
        span_ [class_ "state-label"] "Bob's Pre-Sig (σ̃ᴮ)"
        stateValueInput_ (formatHex rSign <> " || " <> formatHex sigTilde)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Bob's Pre-Sig (σ̃ᴮ)"
        span_ [class_ "state-value"] ""
  -- Pre-Signature Verification Status
  if psPreSignatureVerified partyState
    then div_ [class_ (stateClass (psPreSignatureVerifiedFresh partyState))] do
      span_ [class_ "state-label"] "Pre-Sig Status"
      span_ [class_ "state-value"] "✓ Verified"
    else div_ [class_ "state-item state-unset"] do
      span_ [class_ "state-label"] "Pre-Sig Status"
      span_ [class_ "state-value"] ""

-- | Render Alice's complete state (with OOB wrapper)
renderAliceStateUpdate
  :: Quantity 'Apple -> Quantity 'Banana -> PartyState -> Html ()
renderAliceStateUpdate apples bananas partyState = do
  div_ [hxSwapOob_ "innerHTML:#alice-state"] do
    section_ "State"
    renderAliceState apples bananas partyState

-- | Render Bob's state fields (no OOB wrapper)
renderBobStateFields :: PartyState -> Html ()
renderBobStateFields partyState = do
  let stateClass isFresh =
        if isFresh
          then "state-item state-set recently-updated"
          else "state-item state-set"
  -- Private Key (sk0, sk1)
  case SM.maybe Nothing Just (psPrivateKey partyState) of
    Just (Ed25519PrivateKey (PrivateKey sk0) sk1) -> do
      div_ [class_ (stateClass (psPrivateKeyFresh partyState))] do
        span_ [class_ "state-label"] "Private Key (sk0)"
        stateValueInput_ (formatHex sk0)
      div_ [class_ (stateClass (psPrivateKeyFresh partyState))] do
        span_ [class_ "state-label"] "Private Key (sk1)"
        stateValueInput_ (formatHex sk1)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Private Key (sk0)"
        span_ [class_ "state-value"] ""
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Private Key (sk1)"
        span_ [class_ "state-value"] ""
  -- Public Key
  case SM.maybe Nothing Just (psPublicKey partyState) of
    Just (PublicKey pkBytes) -> do
      div_ [class_ (stateClass (psPublicKeyFresh partyState))] do
        span_ [class_ "state-label"] "Public Key"
        stateValueInput_ (formatHex pkBytes)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Public Key"
        span_ [class_ "state-value"] ""
  -- Alice's Public Key (received from Alice)
  case SM.maybe Nothing Just (psOtherPartyPublicKey partyState) of
    Just (PublicKey pkBytes) -> do
      div_ [class_ (stateClass (psOtherPartyPublicKeyFresh partyState))] do
        span_ [class_ "state-label"] "Alice's Public Key"
        stateValueInput_ (formatHex pkBytes)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Alice's Public Key"
        span_ [class_ "state-value"] ""
  -- Alice's Commitment (received from Alice)
  case SM.maybe Nothing Just (psOtherPartyCommitment partyState) of
    Just (AdapterPoint commitBytes) -> do
      div_ [class_ (stateClass (psOtherPartyCommitmentFresh partyState))] do
        span_ [class_ "state-label"] "Alice's Seal (Y)"
        stateValueInput_ (formatHex commitBytes)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Alice's Seal (Y)"
        span_ [class_ "state-value"] ""
  -- Alice's Seal Proof (received from Alice)
  case SM.maybe Nothing Just (psOtherPartyNIZKProof partyState) of
    Just (NIZKProof proofBytes) -> do
      div_ [class_ (stateClass (psOtherPartyNIZKProofFresh partyState))] do
        span_ [class_ "state-label"] "Alice's Seal Proof"
        stateValueInput_ (formatHex proofBytes)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Alice's Seal Proof"
        span_ [class_ "state-value"] ""
  -- Seal Proof Verification Status
  if psNIZKProofVerified partyState
    then div_ [class_ (stateClass (psNIZKProofVerifiedFresh partyState))] do
      span_ [class_ "state-label"] "Seal Proof Status"
      span_ [class_ "state-value"] "✓ Verified"
    else div_ [class_ "state-item state-unset"] do
      span_ [class_ "state-label"] "Seal Proof Status"
      span_ [class_ "state-value"] ""
  -- Bob's Own Transaction
  case SM.maybe Nothing Just (psTransaction partyState) of
    Just tx -> do
      div_ [class_ (stateClass (psTransactionFresh partyState))] do
        span_ [class_ "state-label"] "Transaction"
        span_ [class_ "state-value"] $
          toHtml
            ( show (length (txInputs tx))
                <> " inputs, "
                <> show (length (txOutputs tx))
                <> " outputs"
                :: Text
            )
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Transaction"
        span_ [class_ "state-value"] ""
  -- Bob's Own Pre-Signature
  case SM.maybe Nothing Just (psPreSignature partyState) of
    Just (AdaptedSignature rSign sigTilde) -> do
      div_ [class_ (stateClass (psPreSignatureFresh partyState))] do
        span_ [class_ "state-label"] "Pre-Signature (σ̃ᴮ)"
        stateValueInput_ (formatHex rSign <> " || " <> formatHex sigTilde)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Pre-Signature (σ̃ᴮ)"
        span_ [class_ "state-value"] ""
  -- Alice's Transaction (received from Alice)
  case SM.maybe Nothing Just (psOtherPartyTransaction partyState) of
    Just tx -> do
      div_ [class_ (stateClass (psOtherPartyTransactionFresh partyState))] do
        span_ [class_ "state-label"] "Alice's Transaction"
        span_ [class_ "state-value"] $
          toHtml
            ( show (length (txInputs tx))
                <> " inputs, "
                <> show (length (txOutputs tx))
                <> " outputs"
                :: Text
            )
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Alice's Transaction"
        span_ [class_ "state-value"] ""
  -- Alice's Pre-Signature (received from Alice)
  case SM.maybe Nothing Just (psOtherPartyPreSignature partyState) of
    Just (AdaptedSignature rSign sigTilde) -> do
      div_ [class_ (stateClass (psOtherPartyPreSignatureFresh partyState))] do
        span_ [class_ "state-label"] "Alice's Pre-Sig (σ̃ᴬ)"
        stateValueInput_ (formatHex rSign <> " || " <> formatHex sigTilde)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Alice's Pre-Sig (σ̃ᴬ)"
        span_ [class_ "state-value"] ""
  -- Extracted Secret (Bob only)
  case SM.maybe Nothing Just (psExtractedSecret partyState) of
    Just (AdapterSecret secretBytes) -> do
      div_ [class_ (stateClass (psExtractedSecretFresh partyState))] do
        span_ [class_ "state-label"] "Extracted Secret (y)"
        stateValueInput_ (formatHex secretBytes)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Extracted Secret (y)"
        span_ [class_ "state-value"] ""
  -- Bob's Complete Signature
  case SM.maybe Nothing Just (psCompleteSignature partyState) of
    Just (Signature rSign sig) -> do
      div_ [class_ (stateClass (psCompleteSignatureFresh partyState))] do
        span_ [class_ "state-label"] "Complete Sig (σᴮ)"
        stateValueInput_ (formatHex rSign <> " || " <> formatHex sig)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Complete Sig (σᴮ)"
        span_ [class_ "state-value"] ""

-- | Render Bob's complete state (with OOB wrapper)
renderBobStateUpdate
  :: Quantity 'Apple -> Quantity 'Banana -> PartyState -> Html ()
renderBobStateUpdate apples bananas partyState = do
  div_ [hxSwapOob_ "innerHTML:#bob-state"] do
    section_ "State"
    renderBobState apples bananas partyState

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Blockchain Visualization ----------------------------------------------------

-- | Render Alice's blockchain view (Apple-chain UTXOs and transactions)
renderAliceBlockchain
  :: Quantity 'Apple -> Quantity 'Banana -> PartyState -> Html ()
renderAliceBlockchain (Quantity apples) _bananas partyState = do
  -- UTXOs section (Alice's blockchain = Apple-chain)
  div_ [class_ "blockchain-utxos"] do
    div_ [class_ "blockchain-label"] "UTXOs"
    div_ [class_ "utxo-list"] do
      -- Apples ownership: Alice before publishing, Bob after
      case psCompleteSignature partyState of
        SM.Nothing ->
          div_ [class_ "utxo-item"] $
            toHtml (show apples <> " 🍎 (owned by Alice)" :: Text)
        SM.Just _ ->
          div_ [class_ "utxo-item utxo-transferred"] $
            toHtml (show apples <> " 🍎 (owned by Bob)" :: Text)

  -- Transactions section (Alice publishes on AppleChain)
  div_ [class_ "blockchain-txs"] do
    div_ [class_ "blockchain-label"] "Transactions"
    case psCompleteSignature partyState of
      SM.Just _ ->
        div_ [class_ "tx-item tx-published"] do
          span_ [class_ "tx-status"] "✓"
          span_ [class_ "tx-description"] $
            toHtml ("Alice → Bob (" <> show apples <> " 🍎)" :: Text)
      SM.Nothing ->
        div_ [class_ "tx-empty"] "No published transactions"

-- | Render Bob's blockchain view (Banana-chain UTXOs and transactions)
renderBobBlockchain
  :: Quantity 'Apple -> Quantity 'Banana -> PartyState -> Html ()
renderBobBlockchain _apples (Quantity bananas) partyState = do
  -- UTXOs section (Bob's blockchain = Banana-chain)
  div_ [class_ "blockchain-utxos"] do
    div_ [class_ "blockchain-label"] "UTXOs"
    div_ [class_ "utxo-list"] do
      -- Bananas ownership: Bob before publishing, Alice after
      case psCompleteSignature partyState of
        SM.Nothing ->
          div_ [class_ "utxo-item"] $
            toHtml (show bananas <> " 🍌 (owned by Bob)" :: Text)
        SM.Just _ ->
          div_ [class_ "utxo-item utxo-transferred"] $
            toHtml (show bananas <> " 🍌 (owned by Alice)" :: Text)

  -- Transactions section (Bob publishes on BananaChain)
  div_ [class_ "blockchain-txs"] do
    div_ [class_ "blockchain-label"] "Transactions"
    case psCompleteSignature partyState of
      SM.Just _ ->
        div_ [class_ "tx-item tx-published"] do
          span_ [class_ "tx-status"] "✓"
          span_ [class_ "tx-description"] $
            toHtml ("Bob → Alice (" <> show bananas <> " 🍌)" :: Text)
      SM.Nothing ->
        div_ [class_ "tx-empty"] "No published transactions"

-- | Render Alice's blockchain with OOB wrapper
renderAliceBlockchainUpdate
  :: Quantity 'Apple -> Quantity 'Banana -> PartyState -> Html ()
renderAliceBlockchainUpdate apples bananas partyState = do
  div_ [hxSwapOob_ "innerHTML:#alice-blockchain"] do
    section_ "Blockchain"
    renderAliceBlockchain apples bananas partyState

-- | Render Bob's blockchain with OOB wrapper
renderBobBlockchainUpdate
  :: Quantity 'Apple -> Quantity 'Banana -> PartyState -> Html ()
renderBobBlockchainUpdate apples bananas partyState = do
  div_ [hxSwapOob_ "innerHTML:#bob-blockchain"] do
    section_ "Blockchain"
    renderBobBlockchain apples bananas partyState

-- | Render Alice's complete state with transaction formatting
renderAliceState :: Quantity 'Apple -> Quantity 'Banana -> PartyState -> Html ()
renderAliceState apples bananas = renderAliceStateFieldsWithAmounts apples bananas

-- | Render Bob's complete state with transaction formatting
renderBobState :: Quantity 'Apple -> Quantity 'Banana -> PartyState -> Html ()
renderBobState apples bananas = renderBobStateFieldsWithAmounts apples bananas

-- | Alice's state with proper transaction formatting
renderAliceStateFieldsWithAmounts
  :: Quantity 'Apple -> Quantity 'Banana -> PartyState -> Html ()
renderAliceStateFieldsWithAmounts (Quantity apples) (Quantity bananas) partyState = do
  let stateClass isFresh =
        if isFresh
          then "state-item state-set recently-updated"
          else "state-item state-set"
  -- Private Key (sk0, sk1)
  case SM.maybe Nothing Just (psPrivateKey partyState) of
    Just (Ed25519PrivateKey (PrivateKey sk0) sk1) -> do
      div_ [class_ (stateClass (psPrivateKeyFresh partyState))] do
        span_ [class_ "state-label"] "Private Key (sk0)"
        stateValueInput_ (formatHex sk0)
      div_ [class_ (stateClass (psPrivateKeyFresh partyState))] do
        span_ [class_ "state-label"] "Private Key (sk1)"
        stateValueInput_ (formatHex sk1)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Private Key (sk0)"
        span_ [class_ "state-value"] ""
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Private Key (sk1)"
        span_ [class_ "state-value"] ""
  -- Public Key
  case SM.maybe Nothing Just (psPublicKey partyState) of
    Just (PublicKey pkBytes) -> do
      div_ [class_ (stateClass (psPublicKeyFresh partyState))] do
        span_ [class_ "state-label"] "Public Key"
        stateValueInput_ (formatHex pkBytes)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Public Key"
        span_ [class_ "state-value"] ""
  -- Bob's Public Key (received from Bob)
  case SM.maybe Nothing Just (psOtherPartyPublicKey partyState) of
    Just (PublicKey pkBytes) -> do
      div_ [class_ (stateClass (psOtherPartyPublicKeyFresh partyState))] do
        span_ [class_ "state-label"] "Bob's Public Key"
        stateValueInput_ (formatHex pkBytes)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Bob's Public Key"
        span_ [class_ "state-value"] ""
  -- Adapter Secret
  case SM.maybe Nothing Just (psAdapterSecret partyState) of
    Just (AdapterSecret secretBytes) -> do
      div_ [class_ (stateClass (psAdapterSecretFresh partyState))] do
        span_ [class_ "state-label"] "Adapter Secret (y)"
        stateValueInput_ (formatHex secretBytes)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Adapter Secret (y)"
        span_ [class_ "state-value"] ""
  -- Commitment
  case SM.maybe Nothing Just (psAdapterCommitment partyState) of
    Just (AdapterPoint commitBytes) -> do
      div_ [class_ (stateClass (psAdapterCommitmentFresh partyState))] do
        span_ [class_ "state-label"] "Seal (Y)"
        stateValueInput_ (formatHex commitBytes)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Seal (Y)"
        span_ [class_ "state-value"] ""
  -- Seal Proof
  case SM.maybe Nothing Just (psNIZKProof partyState) of
    Just (NIZKProof proofBytes) -> do
      div_ [class_ (stateClass (psNIZKProofFresh partyState))] do
        span_ [class_ "state-label"] "Seal Proof"
        stateValueInput_ (formatHex proofBytes)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Seal Proof"
        span_ [class_ "state-value"] ""
  -- Transaction (Alice's own)
  case SM.maybe Nothing Just (psTransaction partyState) of
    Just _tx -> do
      div_ [class_ (stateClass (psTransactionFresh partyState))] do
        span_ [class_ "state-label"] "Transaction"
        span_ [class_ "state-value"] $
          toHtml ("Alice → Bob (" <> show apples <> " 🍎)" :: Text)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Transaction"
        span_ [class_ "state-value"] ""
  -- Pre-Signature
  case SM.maybe Nothing Just (psPreSignature partyState) of
    Just (AdaptedSignature rSign sigTilde) -> do
      div_ [class_ (stateClass (psPreSignatureFresh partyState))] do
        span_ [class_ "state-label"] "Pre-Signature (σ̃)"
        stateValueInput_ (formatHex rSign <> " || " <> formatHex sigTilde)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Pre-Signature (σ̃)"
        span_ [class_ "state-value"] ""
  -- Alice's Complete Signature
  case SM.maybe Nothing Just (psCompleteSignature partyState) of
    Just (Signature rSign sig) -> do
      div_ [class_ (stateClass (psCompleteSignatureFresh partyState))] do
        span_ [class_ "state-label"] "Complete Sig (σᴬ)"
        stateValueInput_ (formatHex rSign <> " || " <> formatHex sig)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Complete Sig (σᴬ)"
        span_ [class_ "state-value"] ""
  -- Bob's Transaction (received from Bob)
  case SM.maybe Nothing Just (psOtherPartyTransaction partyState) of
    Just _tx -> do
      div_ [class_ (stateClass (psOtherPartyTransactionFresh partyState))] do
        span_ [class_ "state-label"] "Bob's Transaction"
        span_ [class_ "state-value"] $
          toHtml ("Bob → Alice (" <> show bananas <> " 🍌)" :: Text)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Bob's Transaction"
        span_ [class_ "state-value"] ""
  -- Bob's Pre-Signature (received from Bob)
  case SM.maybe Nothing Just (psOtherPartyPreSignature partyState) of
    Just (AdaptedSignature rSign sigTilde) -> do
      div_ [class_ (stateClass (psOtherPartyPreSignatureFresh partyState))] do
        span_ [class_ "state-label"] "Bob's Pre-Sig (σ̃ᴮ)"
        stateValueInput_ (formatHex rSign <> " || " <> formatHex sigTilde)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Bob's Pre-Sig (σ̃ᴮ)"
        span_ [class_ "state-value"] ""
  -- Pre-Signature Verification Status
  if psPreSignatureVerified partyState
    then div_ [class_ (stateClass (psPreSignatureVerifiedFresh partyState))] do
      span_ [class_ "state-label"] "Pre-Sig Status"
      span_ [class_ "state-value"] "✓ Verified"
    else div_ [class_ "state-item state-unset"] do
      span_ [class_ "state-label"] "Pre-Sig Status"
      span_ [class_ "state-value"] ""

-- | Bob's state with proper transaction formatting
renderBobStateFieldsWithAmounts
  :: Quantity 'Apple -> Quantity 'Banana -> PartyState -> Html ()
renderBobStateFieldsWithAmounts (Quantity apples) (Quantity bananas) partyState = do
  let stateClass isFresh =
        if isFresh
          then "state-item state-set recently-updated"
          else "state-item state-set"
  -- Private Key (sk0, sk1)
  case SM.maybe Nothing Just (psPrivateKey partyState) of
    Just (Ed25519PrivateKey (PrivateKey sk0) sk1) -> do
      div_ [class_ (stateClass (psPrivateKeyFresh partyState))] do
        span_ [class_ "state-label"] "Private Key (sk0)"
        stateValueInput_ (formatHex sk0)
      div_ [class_ (stateClass (psPrivateKeyFresh partyState))] do
        span_ [class_ "state-label"] "Private Key (sk1)"
        stateValueInput_ (formatHex sk1)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Private Key (sk0)"
        span_ [class_ "state-value"] ""
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Private Key (sk1)"
        span_ [class_ "state-value"] ""
  -- Public Key
  case SM.maybe Nothing Just (psPublicKey partyState) of
    Just (PublicKey pkBytes) -> do
      div_ [class_ (stateClass (psPublicKeyFresh partyState))] do
        span_ [class_ "state-label"] "Public Key"
        stateValueInput_ (formatHex pkBytes)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Public Key"
        span_ [class_ "state-value"] ""
  -- Alice's Public Key (received from Alice)
  case SM.maybe Nothing Just (psOtherPartyPublicKey partyState) of
    Just (PublicKey pkBytes) -> do
      div_ [class_ (stateClass (psOtherPartyPublicKeyFresh partyState))] do
        span_ [class_ "state-label"] "Alice's Public Key"
        stateValueInput_ (formatHex pkBytes)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Alice's Public Key"
        span_ [class_ "state-value"] ""
  -- Alice's Commitment (received from Alice)
  case SM.maybe Nothing Just (psOtherPartyCommitment partyState) of
    Just (AdapterPoint commitBytes) -> do
      div_ [class_ (stateClass (psOtherPartyCommitmentFresh partyState))] do
        span_ [class_ "state-label"] "Alice's Seal (Y)"
        stateValueInput_ (formatHex commitBytes)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Alice's Seal (Y)"
        span_ [class_ "state-value"] ""
  -- Alice's Seal Proof (received from Alice)
  case SM.maybe Nothing Just (psOtherPartyNIZKProof partyState) of
    Just (NIZKProof proofBytes) -> do
      div_ [class_ (stateClass (psOtherPartyNIZKProofFresh partyState))] do
        span_ [class_ "state-label"] "Alice's Seal Proof"
        stateValueInput_ (formatHex proofBytes)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Alice's Seal Proof"
        span_ [class_ "state-value"] ""
  -- Seal Proof Verification Status
  if psNIZKProofVerified partyState
    then div_ [class_ (stateClass (psNIZKProofVerifiedFresh partyState))] do
      span_ [class_ "state-label"] "Seal Proof Status"
      span_ [class_ "state-value"] "✓ Verified"
    else div_ [class_ "state-item state-unset"] do
      span_ [class_ "state-label"] "Seal Proof Status"
      span_ [class_ "state-value"] ""
  -- Bob's Own Transaction
  case SM.maybe Nothing Just (psTransaction partyState) of
    Just _tx -> do
      div_ [class_ (stateClass (psTransactionFresh partyState))] do
        span_ [class_ "state-label"] "Transaction"
        span_ [class_ "state-value"] $
          toHtml ("Bob → Alice (" <> show bananas <> " 🍌)" :: Text)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Transaction"
        span_ [class_ "state-value"] ""
  -- Bob's Own Pre-Signature
  case SM.maybe Nothing Just (psPreSignature partyState) of
    Just (AdaptedSignature rSign sigTilde) -> do
      div_ [class_ (stateClass (psPreSignatureFresh partyState))] do
        span_ [class_ "state-label"] "Pre-Signature (σ̃ᴮ)"
        stateValueInput_ (formatHex rSign <> " || " <> formatHex sigTilde)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Pre-Signature (σ̃ᴮ)"
        span_ [class_ "state-value"] ""
  -- Alice's Transaction (received from Alice)
  case SM.maybe Nothing Just (psOtherPartyTransaction partyState) of
    Just _tx -> do
      div_ [class_ (stateClass (psOtherPartyTransactionFresh partyState))] do
        span_ [class_ "state-label"] "Alice's Transaction"
        span_ [class_ "state-value"] $
          toHtml ("Alice → Bob (" <> show apples <> " 🍎)" :: Text)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Alice's Transaction"
        span_ [class_ "state-value"] ""
  -- Alice's Pre-Signature (received from Alice)
  case SM.maybe Nothing Just (psOtherPartyPreSignature partyState) of
    Just (AdaptedSignature rSign sigTilde) -> do
      div_ [class_ (stateClass (psOtherPartyPreSignatureFresh partyState))] do
        span_ [class_ "state-label"] "Alice's Pre-Sig (σ̃ᴬ)"
        stateValueInput_ (formatHex rSign <> " || " <> formatHex sigTilde)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Alice's Pre-Sig (σ̃ᴬ)"
        span_ [class_ "state-value"] ""
  -- Extracted Secret (Bob only)
  case SM.maybe Nothing Just (psExtractedSecret partyState) of
    Just (AdapterSecret secretBytes) -> do
      div_ [class_ (stateClass (psExtractedSecretFresh partyState))] do
        span_ [class_ "state-label"] "Extracted Secret (y)"
        stateValueInput_ (formatHex secretBytes)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Extracted Secret (y)"
        span_ [class_ "state-value"] ""
  -- Bob's Complete Signature
  case SM.maybe Nothing Just (psCompleteSignature partyState) of
    Just (Signature rSign sig) -> do
      div_ [class_ (stateClass (psCompleteSignatureFresh partyState))] do
        span_ [class_ "state-label"] "Complete Sig (σᴮ)"
        stateValueInput_ (formatHex rSign <> " || " <> formatHex sig)
    Nothing -> do
      div_ [class_ "state-item state-unset"] do
        span_ [class_ "state-label"] "Complete Sig (σᴮ)"
        span_ [class_ "state-value"] ""
