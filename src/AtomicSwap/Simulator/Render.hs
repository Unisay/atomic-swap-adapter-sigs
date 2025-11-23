{-# LANGUAGE StrictData #-}

{- |
Module: AtomicSwap.Simulator.Render
Description: HTML rendering for simulator UI (presentation layer)

This module provides HTML rendering functions that correspond to step executors.
Follows ELM architecture: renderers take full PartyState and render complete UI
using HTMX out-of-band updates for targeted DOM sections.

Correspondence with Steps module:
- executeAliceKeygen → renderStep with "Alice generated Ed25519 keypair"
- executeAliceGenerateSecret → renderStep with "Alice generated adapter secret y"
- executeAliceMakeCommitment → renderStep with "Alice computed commitment Y = y·B"
- executeBobKeygen → renderStep with "Bob generated Ed25519 keypair"
-}
module AtomicSwap.Simulator.Render
  ( -- * Page Templates
    mainPage

    -- * State-Diffing Renderers
  , renderStateUpdates
  , renderNewTimelineEntry
  ) where

import Lucid
  ( Attributes
  , Html
  , body_
  , button_
  , charset_
  , class_
  , content_
  , div_
  , doctypehtml_
  , h1_
  , h2_
  , head_
  , header_
  , href_
  , id_
  , img_
  , link_
  , meta_
  , name_
  , p_
  , rel_
  , script_
  , span_
  , src_
  , strong_
  , style_
  , term
  , title_
  , toHtml
  )

import Data.ByteString.Base16 qualified as Base16
import Data.Strict.Maybe qualified as SM
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Prelude

import AtomicSwap.Simulator.Actions
  ( ActionMetadata (..)
  , actionMetadata
  , availableActions
  , recommendedAction
  )
import AtomicSwap.Simulator.State
  ( PartyState (..)
  , SimulatorState (..)
  , getPartyState
  )
import AtomicSwap.Simulator.Types
  ( Action
  , Asset (..)
  , GlobalState
  , Participant (..)
  , Quantity (..)
  , StateUpdate (..)
  , StepEntry
  , StepIndex (..)
  )
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

--------------------------------------------------------------------------------
-- Helper Functions ------------------------------------------------------------

-- | Section header with horizontal line and overlaid label
section_ :: Text -> Html ()
section_ label =
  div_ [class_ "section-header"] do
    span_ [class_ "section-label"] (toHtml label)

-- | Icon element (renders SVG from static/icons/)
stepIcon_ :: Text -> Html ()
stepIcon_ iconName =
  img_ [class_ "step-icon", src_ ("/static/icons/" <> iconName <> ".svg")]

-- | Determine icon based on step description keywords
getStepIcon :: Text -> Text
getStepIcon description
  | "generated Ed25519 keypair" `T.isInfixOf` description = "key"
  | "generated adapter secret" `T.isInfixOf` description = "lock"
  | "computed commitment" `T.isInfixOf` description = "lock"
  | "shared" `T.isInfixOf` description = "arrow-right"
  | "generated seal proof" `T.isInfixOf` description = "badge-check"
  | "verified" `T.isInfixOf` description = "check-circle"
  | "prepared transaction" `T.isInfixOf` description = "card-wallet"
  | "created adapter pre-signature" `T.isInfixOf` description = "lock"
  | "published transaction" `T.isInfixOf` description = "card-wallet"
  | "completed signature" `T.isInfixOf` description = "check-circle"
  | "extracted adapter secret" `T.isInfixOf` description = "key"
  | otherwise = "check-circle" -- Default icon

-- | Get explanation content for a step (if available)
getStepExplanation :: Text -> Text -> Maybe (Html ())
getStepExplanation stepId description
  | "generated Ed25519 keypair" `T.isInfixOf` description =
      Just (keypairExplanation stepId)
  | "shared her public key" `T.isInfixOf` description =
      Just (sharedPublicKeyExplanation stepId)
  | "shared his public key" `T.isInfixOf` description =
      Just (sharedPublicKeyExplanation stepId)
  | "generated adapter secret" `T.isInfixOf` description =
      Just (adapterSecretExplanation stepId)
  | "computed commitment Y" `T.isInfixOf` description =
      Just (commitmentExplanation stepId)
  | "shared commitment Y" `T.isInfixOf` description =
      Just (sharedCommitmentExplanation stepId)
  | "generated seal proof" `T.isInfixOf` description =
      Just (sealProofExplanation stepId)
  | "shared seal proof" `T.isInfixOf` description =
      Just (sharedSealProofExplanation stepId)
  | "verified Alice's seal proof" `T.isInfixOf` description =
      Just (verifiedSealProofExplanation stepId)
  | "prepared transaction" `T.isInfixOf` description =
      Just (preparedTransactionExplanation stepId)
  | "created adapter pre-signature" `T.isInfixOf` description =
      Just (adapterPreSignatureExplanation stepId)
  | "published transaction and pre-signature" `T.isInfixOf` description =
      Just (publishedTxAndPreSigExplanation stepId)
  | "verified Alice's pre-signature" `T.isInfixOf` description =
      Just (verifiedPreSignatureExplanation stepId "Alice")
  | "verified Bob's pre-signature" `T.isInfixOf` description =
      Just (verifiedPreSignatureExplanation stepId "Bob")
  | "completed signature and published to blockchain (reveals secret y!)"
      `T.isInfixOf` description =
      Just (completedSignatureAliceExplanation stepId)
  | "extracted adapter secret" `T.isInfixOf` description =
      Just (extractedSecretExplanation stepId)
  | "completed signature and published to blockchain" `T.isInfixOf` description
      && not ("extracted" `T.isInfixOf` T.toLower description) =
      Just (completedSignatureBobExplanation stepId)
  | otherwise = Nothing
  where
    keypairExplanation sid = do
      p_
        "Each participant generates an Ed25519 keypair for signing transactions. This keypair will be used throughout the protocol to authenticate actions."
      renderQA
        sid
        "qa-what-is-ed25519"
        "What is Ed25519?"
        do
          p_
            "Ed25519 is a modern elliptic curve signature scheme known for its speed and strong security guarantees. It's widely used in cryptocurrencies and secure protocols."
      renderQA
        sid
        "qa-why-two-keys"
        "Why two keys (sk0 and sk1) instead of one?"
        do
          p_ do
            "This split structure is specific to "
            strong_ "rEdDSA"
            " (randomized EdDSA). The "
            term "code" [] "sk0"
            " component is the signing scalar, while "
            term "code" [] "sk1"
            " is the nonce seed. This design introduces additional randomness during signing, preventing certain attack vectors where an attacker might try to exploit predictable nonce generation."

    sharedPublicKeyExplanation sid = do
      p_
        "Public keys must be exchanged before the protocol can proceed. Each participant needs the other's public key to verify signatures and construct transactions."
      renderQA
        sid
        "qa-safe-to-share-pubkey"
        "Is it safe to share the public key?"
        do
          p_
            "Yes! The public key is derived from the private key and is meant to be shared publicly. Only the holder of the private key can create valid signatures—the public key alone cannot be used to forge signatures or derive the private key."
      renderQA
        sid
        "qa-why-share-pubkey"
        "Why do we need to share public keys?"
        do
          p_
            "Public keys serve two critical purposes in the protocol: First, they allow each party to verify the other's signatures on transactions. Second, they're needed to construct the correct transaction outputs—each party needs to know where to send the funds (to the other party's public key)."

    adapterSecretExplanation sid = do
      p_ do
        "Alice generates a random secret "
        term "code" [] "y"
        " called the adapter secret. This secret is the key to the atomic swap—whoever knows "
        term "code" [] "y"
        " can complete both transactions."
      renderQA
        sid
        "qa-what-adapter-secret"
        "What is the adapter secret?"
        do
          p_ do
            "The adapter secret "
            term "code" [] "y"
            " is a random scalar value that Alice will use to \"lock\" both transactions together. When Alice eventually reveals "
            term "code" [] "y"
            " by publishing her complete signature, Bob can extract it and use it to complete his own transaction. This creates the atomic property: either both transactions complete, or neither does."
      renderQA
        sid
        "qa-why-alice-only"
        "Why does only Alice generate this secret?"
        do
          p_
            "In this protocol, Alice initiates the swap and goes first. She generates the adapter secret and creates the \"lock\" that binds both transactions together. Bob will verify Alice's commitments but doesn't need his own adapter secret—he'll extract Alice's secret from her published signature."
      renderQA
        sid
        "qa-lose-secret"
        "What if Alice loses this secret?"
        do
          p_ do
            "If Alice loses "
            term "code" [] "y"
            " before completing the protocol, she won't be able to complete her signature and claim Bob's funds. However, Bob also won't be able to complete his transaction (since he needs "
            term "code" [] "y"
            " too), so the swap simply fails without anyone losing funds."

    commitmentExplanation sid = do
      p_ do
        "Alice computes the commitment "
        term "code" [] "Y = y·B"
        " by multiplying her secret "
        term "code" [] "y"
        " with the Ed25519 base point "
        term "code" [] "B"
        ". This commitment will be shared with Bob."
      renderQA
        sid
        "qa-what-commitment"
        "What is a commitment?"
        do
          p_ do
            "A commitment is a cryptographic way to \"seal\" a value without revealing it. The commitment "
            term "code" [] "Y"
            " proves Alice knows some secret "
            term "code" [] "y"
            " without telling anyone what "
            term "code" [] "y"
            " is. Later, when Alice reveals "
            term "code" [] "y"
            ", everyone can verify it matches the commitment."
      renderQA
        sid
        "qa-why-y-times-b"
        "Why compute Y = y·B specifically?"
        do
          p_ do
            "Multiplying by the base point "
            term "code" [] "B"
            " on the Ed25519 curve creates a one-way function: it's easy to compute "
            term "code" [] "Y"
            " from "
            term "code" [] "y"
            ", but computationally impossible to reverse and find "
            term "code" [] "y"
            " from "
            term "code" [] "Y"
            ". This is the same mathematical operation used to derive public keys from private keys."
      renderQA
        sid
        "qa-reverse-commitment"
        "Can anyone reverse this to find y?"
        do
          p_ do
            "No. Finding "
            term "code" [] "y"
            " from "
            term "code" [] "Y"
            " would require solving the discrete logarithm problem on the Ed25519 curve, which is computationally infeasible with current technology. This is what makes the commitment secure."

    sharedCommitmentExplanation sid = do
      p_ do
        "Alice shares her commitment "
        term "code" [] "Y"
        " with Bob. This allows Bob to verify that Alice's pre-signatures are constructed correctly, without revealing the secret "
        term "code" [] "y"
        "."
      renderQA
        sid
        "qa-safe-share-commitment"
        "Is it safe to share Y?"
        do
          p_ do
            "Yes! The commitment "
            term "code" [] "Y"
            " reveals nothing about the secret "
            term "code" [] "y"
            ". It's mathematically impossible to extract "
            term "code" [] "y"
            " from "
            term "code" [] "Y"
            " alone. However, "
            term "code" [] "Y"
            " will later allow Bob to verify adapter signatures and extract "
            term "code" [] "y"
            " once Alice publishes her complete signature."
      renderQA
        sid
        "qa-why-bob-need-commitment"
        "Why does Bob need Y?"
        do
          p_ do
            "Bob needs "
            term "code" [] "Y"
            " for two critical purposes: First, he'll use it to verify that Alice's adapter pre-signature is correctly constructed (it must \"adapt\" to "
            term "code" [] "Y"
            "). Second, once Alice publishes her complete signature, Bob will use "
            term "code" [] "Y"
            " to extract the secret "
            term "code" [] "y"
            " and complete his own transaction."
      renderQA
        sid
        "qa-what-bob-do-y"
        "What can Bob do with Y alone?"
        do
          p_ do
            "With just "
            term "code" [] "Y"
            ", Bob can verify adapter signatures but cannot complete any transactions. He still needs the actual secret "
            term "code" [] "y"
            " to create complete signatures. Bob will only learn "
            term "code" [] "y"
            " after Alice publishes her complete signature to the blockchain."

    sealProofExplanation sid = do
      p_ do
        "Alice generates a "
        strong_ "zero-knowledge proof"
        " that proves she knows the secret "
        term "code" [] "y"
        " corresponding to the commitment "
        term "code" [] "Y"
        ", without revealing what "
        term "code" [] "y"
        " is. This proof will be shared with Bob."
      renderQA
        sid
        "qa-what-seal-proof"
        "What is a seal proof?"
        do
          p_ do
            "A seal proof (formally called a "
            strong_ "NIZK proof"
            " - Non-Interactive Zero-Knowledge proof) is cryptographic evidence that Alice knows the secret "
            term "code" [] "y"
            " such that "
            term "code" [] "Y = y·B"
            ". The \"zero-knowledge\" property means the proof reveals nothing about "
            term "code" [] "y"
            " itself—Bob learns only that Alice knows it."
      renderQA
        sid
        "qa-why-prove-knowledge"
        "Why does Alice need to prove she knows y?"
        do
          p_ do
            "Without this proof, Alice could send Bob a random commitment "
            term "code" [] "Y"
            " for which she doesn't actually know "
            term "code" [] "y"
            ". Bob would create his transaction based on this fake commitment, but Alice would never be able to complete the swap (since she doesn't know "
            term "code" [] "y"
            "). The proof protects Bob from this attack by ensuring Alice actually possesses the secret."
      renderQA
        sid
        "qa-trust-alice"
        "Can't Bob just trust that Alice knows y?"
        do
          p_
            "No! In cryptographic protocols, we never rely on trust when we can have mathematical proof. Without the seal proof, a malicious Alice could lock Bob's funds without being able to complete her side of the swap. The proof makes the protocol trustless—Bob doesn't need to trust Alice's honesty, only the mathematics."
      renderQA
        sid
        "qa-how-proof-work"
        "How does this proof work?"
        do
          p_ do
            "The proof uses a "
            strong_ "Schnorr sigma protocol"
            " with the Fiat-Shamir transform. Alice essentially demonstrates she can perform operations that would be impossible without knowing "
            term "code" [] "y"
            ", but does so in a way that doesn't leak any information about "
            term "code" [] "y"
            " itself. Bob can verify the proof mathematically and be certain Alice knows the secret."

    sharedSealProofExplanation sid = do
      p_
        "Alice shares the seal proof with Bob so he can verify that she genuinely knows the secret corresponding to the commitment."
      renderQA
        sid
        "qa-why-share-proof"
        "Why does Alice share this proof?"
        do
          p_ do
            "Bob needs the proof to verify that Alice's commitment "
            term "code" [] "Y"
            " is legitimate—that Alice actually knows the secret "
            term "code" [] "y"
            " and can eventually reveal it. Without this verification, Bob would be taking a risk by participating in the swap."
      renderQA
        sid
        "qa-proof-reveal-secret"
        "Does sharing the proof reveal y?"
        do
          p_ do
            "No! That's the magic of zero-knowledge proofs. The proof mathematically demonstrates that Alice knows "
            term "code" [] "y"
            " without revealing any information about what "
            term "code" [] "y"
            " actually is. Bob can verify the proof but cannot extract the secret from it."
      renderQA
        sid
        "qa-what-bob-verify"
        "What will Bob verify with this proof?"
        do
          p_ do
            "Bob will verify that the proof is mathematically valid and correctly links to the commitment "
            term "code" [] "Y"
            " that Alice shared earlier. If verification succeeds, Bob knows with certainty that Alice possesses "
            term "code" [] "y"
            " and can complete the protocol."

    verifiedSealProofExplanation sid = do
      p_
        "Bob verifies Alice's seal proof to confirm she actually knows the secret corresponding to the commitment. This is a critical safety check."
      renderQA
        sid
        "qa-what-verification-check"
        "What does Bob check during verification?"
        do
          p_ do
            "Bob uses the proof, the commitment "
            term "code" [] "Y"
            ", and public parameters to run a mathematical verification algorithm. The algorithm checks that the proof was correctly constructed by someone who knows "
            term "code" [] "y"
            " such that "
            term "code" [] "Y = y·B"
            ". If the verification passes, Bob has mathematical certainty that Alice knows the secret."
      renderQA
        sid
        "qa-verification-fail"
        "What if verification fails?"
        do
          p_
            "If verification fails, Bob should immediately abort the protocol! A failed verification means either Alice doesn't actually know the secret, the proof was corrupted during transmission, or Alice is attempting fraud. Bob must not proceed with creating his transaction or pre-signature if this check fails."
      renderQA
        sid
        "qa-safe-proceed"
        "Why is it now safe for Bob to proceed?"
        do
          p_ do
            "With successful verification, Bob knows Alice can complete her side of the swap. When Alice eventually publishes her complete signature to claim Bob's funds, she will inevitably reveal "
            term "code" [] "y"
            ". Bob can then extract "
            term "code" [] "y"
            " and complete his own transaction. The proof guarantees the swap will be atomic."

    preparedTransactionExplanation sid = do
      p_
        "A blockchain transaction is prepared that will transfer the assets to the other party once properly signed and published."
      renderQA
        sid
        "qa-what-in-transaction"
        "What's in this transaction?"
        do
          p_
            "The transaction specifies inputs (the funds being spent), outputs (where they're going—to the other party's public key), and other blockchain-specific details. It's like writing a check: you specify who pays (inputs), who receives (outputs), and how much, but it's not valid until properly signed."
      renderQA
        sid
        "qa-why-prepare-now"
        "Why prepare the transaction before having all signatures?"
        do
          p_
            "The transaction must be prepared first so both parties can create and verify adapter pre-signatures for the exact same transaction. If Alice and Bob were signing different transactions, the adapter signatures wouldn't link correctly, and the atomic property would break. Both parties need to agree on and know the exact transaction content."
      renderQA
        sid
        "qa-can-change-later"
        "Can the transaction be modified later?"
        do
          p_ do
            "No! Once the transaction is prepared and pre-signatures are created, the transaction cannot be changed. The signatures are cryptographically bound to the exact transaction content. Changing even one byte would invalidate all signatures. This immutability is crucial for security—neither party can change the deal after pre-signatures are exchanged."
      renderQA
        sid
        "qa-transaction-special"
        "What makes this transaction special for atomic swaps?"
        do
          p_ do
            "This transaction will be signed with an "
            strong_ "adapter signature"
            " rather than a normal signature. The adapter signature mechanism is what enables the atomic property: when one party completes their signature and publishes to the blockchain, they automatically reveal the secret "
            term "code" [] "y"
            " that allows the other party to complete their transaction too."

    adapterPreSignatureExplanation sid = do
      p_ do
        "An "
        strong_ "adapter pre-signature"
        " is created—an incomplete but verifiable signature that can be \"completed\" using the adapter secret "
        term "code" [] "y"
        "."
      renderQA
        sid
        "qa-what-adapter-presig"
        "What is an adapter pre-signature?"
        do
          p_ do
            "An adapter pre-signature is an "
            strong_ "incomplete signature"
            " that's \"missing\" the adapter secret "
            term "code" [] "y"
            ". It's constructed so that adding "
            term "code" [] "y"
            " produces a valid, complete signature. Think of it as a locked signature: you can verify it's correct (the lock is real), but you need the key ("
            term "code" [] "y"
            ") to unlock it and use it."
      renderQA
        sid
        "qa-different-normal-sig"
        "How is it different from a normal signature?"
        do
          p_ do
            "A normal signature can be used immediately to authorize a transaction on the blockchain. An adapter pre-signature cannot—it's intentionally incomplete. However, unlike a normal incomplete signature, an adapter pre-signature can be "
            strong_ "verified"
            " before completion. The other party can check that when "
            term "code" [] "y"
            " is added, the result will be a valid signature."
      renderQA
        sid
        "qa-why-incomplete"
        "Why create an incomplete signature?"
        do
          p_ do
            "This is the core of the atomic swap mechanism! By keeping the signature incomplete until the right moment, we create a dependency: you can only complete your signature by revealing "
            term "code" [] "y"
            ", but revealing "
            term "code" [] "y"
            " allows the other party to complete their signature too. This creates the atomic property—either both signatures complete, or neither does."
      renderQA
        sid
        "qa-how-construct"
        "How is it constructed?"
        do
          p_ do
            "Using "
            strong_ "rEdDSA"
            " (randomized EdDSA), the signature is constructed by computing "
            term "code" [] "R_sign = r·B + Y"
            " (the adapted nonce point, which includes the commitment "
            term "code" [] "Y"
            ") and "
            term "code" [] "σ̃ = r + h·sk0"
            " (the incomplete signature scalar, which is missing "
            term "code" [] "y"
            "). To complete it, you add "
            term "code" [] "y"
            " to "
            term "code" [] "σ̃"
            ", giving "
            term "code" [] "σ = σ̃ + y"
            "."

    publishedTxAndPreSigExplanation sid = do
      p_
        "The transaction and adapter pre-signature are shared with the other party so they can verify everything is correct before proceeding."
      renderQA
        sid
        "qa-why-share-both"
        "Why share both transaction and pre-signature together?"
        do
          p_
            "The pre-signature is cryptographically bound to the specific transaction. The other party needs both pieces to verify that the pre-signature is valid for that exact transaction. Sharing them together ensures transparency—both parties can see exactly what transaction will be executed and verify that the cryptographic binding is correct."
      renderQA
        sid
        "qa-safe-share"
        "Is it safe to share the pre-signature?"
        do
          p_ do
            "Yes! The pre-signature is incomplete and cannot be used to claim funds. Even with the transaction and pre-signature, the other party cannot complete the signature without knowing "
            term "code" [] "y"
            ". However, they "
            strong_ "can"
            " verify that the pre-signature is correctly constructed, which is exactly what we want."
      renderQA
        sid
        "qa-what-verify"
        "What can the other party verify?"
        do
          p_ do
            "The other party can verify: (1) The pre-signature is mathematically valid for the given transaction, (2) It's correctly adapted to the commitment "
            term "code" [] "Y"
            ", and (3) When completed with "
            term "code" [] "y"
            ", it will produce a valid signature that can authorize the transaction on the blockchain. This verification doesn't require knowing "
            term "code" [] "y"
            "."

    verifiedPreSignatureExplanation sid party = do
      p_ do
        "The adapter pre-signature is verified to ensure it's correctly constructed and will produce a valid signature when completed with "
        term "code" [] "y"
        "."
      renderQA
        sid
        "qa-verify-incomplete"
        "How can you verify an incomplete signature?"
        do
          p_ do
            "This is the clever part of adapter signatures! Even though the signature is incomplete, the mathematical structure allows verification. The verifier checks that the adapted nonce point "
            term "code" [] "R_sign"
            " and the signature scalar "
            term "code" [] "σ̃"
            " satisfy specific equations that prove: when "
            term "code" [] "y"
            " is added, the result will be a valid signature."
      renderQA
        sid
        "qa-what-guarantees"
        "What guarantees does successful verification provide?"
        do
          p_ do
            "Successful verification guarantees that: (1) The pre-signature was created by someone who knows the private key for the transaction, (2) The pre-signature is correctly adapted to the commitment "
            term "code" [] "Y"
            ", and (3) Completing this pre-signature with "
            term "code" [] "y"
            " will produce a signature that's valid on the blockchain. This means when "
            term "code" [] (party <> "'s")
            " counterparty reveals "
            term "code" [] "y"
            ", "
            term "code" [] party
            " will definitely be able to complete the swap."
      renderQA
        sid
        "qa-fail-verification"
        "What if verification fails?"
        do
          p_ do
            "If verification fails, the protocol must be aborted immediately! A failed verification means the pre-signature is malformed, incorrectly adapted, or fraudulent. Proceeding would risk funds—"
            term "code" [] party
            " might not be able to complete their transaction even after the other party reveals "
            term "code" [] "y"
            ". The atomic property would be broken."

    completedSignatureAliceExplanation sid = do
      p_ do
        strong_ "This is the atomic moment! "
        "Alice completes her signature by adding "
        term "code" [] "y"
        " to her pre-signature and publishes it to the blockchain to claim Bob's funds. By doing so, she inevitably reveals "
        term "code" [] "y"
        " to everyone, including Bob."
      renderQA
        sid
        "qa-complete-signature"
        "How does Alice complete the signature?"
        do
          p_ do
            "Alice computes "
            term "code" [] "σ = σ̃ + y"
            "—she adds her adapter secret "
            term "code" [] "y"
            " to the incomplete signature scalar "
            term "code" [] "σ̃"
            ". The result "
            term "code" [] "σ"
            " is now a complete, valid signature. Combined with the adapted nonce point "
            term "code" [] "R_sign"
            " (which was already public), this forms a complete rEdDSA signature that the blockchain will accept."
      renderQA
        sid
        "qa-reveal-y"
        "How does completing the signature reveal y?"
        do
          p_ do
            "Once the complete signature "
            term "code" [] "(R_sign, σ)"
            " is published to the blockchain, anyone can extract "
            term "code" [] "y"
            " by computing "
            term "code" [] "y = σ - σ̃"
            ". Since "
            term "code" [] "σ"
            " is now public (on the blockchain) and "
            term "code" [] "σ̃"
            " was shared earlier as the pre-signature, the subtraction reveals "
            term "code" [] "y"
            ". There's no way for Alice to claim the funds without revealing the secret."
      renderQA
        sid
        "qa-why-atomic"
        "Why is this the atomic moment?"
        do
          p_ do
            "This is when the swap becomes atomic! Alice cannot claim Bob's funds without publishing the complete signature, and publishing the complete signature automatically reveals "
            term "code" [] "y"
            ". Once "
            term "code" [] "y"
            " is revealed, Bob can immediately extract it and complete his own signature. The atomic property holds: either both parties get their funds, or neither does."
      renderQA
        sid
        "qa-alice-cheat"
        "Can Alice claim funds without revealing y?"
        do
          p_ do
            "No! This is mathematically impossible. To claim the funds, Alice must publish a complete signature to the blockchain. A complete signature requires adding "
            term "code" [] "y"
            " to "
            term "code" [] "σ̃"
            ". Once published, anyone can compute "
            term "code" [] "y = σ - σ̃"
            ". The mathematics ensures Alice cannot cheat—claiming funds and revealing "
            term "code" [] "y"
            " are inseparable."

    extractedSecretExplanation sid = do
      p_ do
        "Bob extracts the adapter secret "
        term "code" [] "y"
        " by watching the blockchain for Alice's complete signature and subtracting the pre-signature he received earlier."
      renderQA
        sid
        "qa-extract-y"
        "How does Bob extract y from the signature?"
        do
          p_ do
            "Bob computes "
            term "code" [] "y = σ - σ̃"
            ", where "
            term "code" [] "σ"
            " is the signature scalar from Alice's complete signature (now public on the blockchain) and "
            term "code" [] "σ̃"
            " is the pre-signature scalar Alice shared earlier. This simple subtraction reveals the adapter secret."
      renderQA
        sid
        "qa-why-now"
        "Why couldn't Bob extract y before?"
        do
          p_ do
            "Before Alice published her complete signature, Bob only had the incomplete pre-signature "
            term "code" [] "σ̃"
            ". The complete signature scalar "
            term "code" [] "σ"
            " was not yet public—it only appeared on the blockchain when Alice claimed her funds. Without "
            term "code" [] "σ"
            ", Bob couldn't perform the subtraction to reveal "
            term "code" [] "y"
            "."
      renderQA
        sid
        "qa-what-do-with-y"
        "What can Bob do with y now?"
        do
          p_ do
            "With "
            term "code" [] "y"
            ", Bob can now complete his own adapter pre-signature! He adds "
            term "code" [] "y"
            " to his pre-signature scalar to create a complete, valid signature for his transaction. This allows him to publish his transaction to the blockchain and claim Alice's funds, completing the atomic swap."
      renderQA
        sid
        "qa-extraction-guaranteed"
        "Is extraction guaranteed to work?"
        do
          p_ do
            "Yes, if Alice successfully verified Bob's pre-signature earlier. The pre-signature verification guaranteed that Bob's pre-signature was correctly adapted to "
            term "code" [] "Y = y·B"
            ". Once Alice reveals "
            term "code" [] "y"
            ", the extracted value will definitely complete Bob's signature. The mathematics ensures this—it's not a matter of luck or timing."

    completedSignatureBobExplanation sid = do
      p_ do
        "Bob completes his signature using the extracted secret "
        term "code" [] "y"
        " and publishes it to the blockchain to claim Alice's funds. The atomic swap is now complete!"
      renderQA
        sid
        "qa-complete-bob-sig"
        "How does Bob complete his signature?"
        do
          p_ do
            "Bob adds the extracted secret "
            term "code" [] "y"
            " to his pre-signature scalar: "
            term "code" [] "σ_Bob = σ̃_Bob + y"
            ". This produces a complete, valid signature that the blockchain will accept. The process is identical to how Alice completed her signature, but Bob is using the "
            strong_ "extracted"
            " value of "
            term "code" [] "y"
            " rather than generating it himself."
      renderQA
        sid
        "qa-why-need-y"
        "Why does Bob need y to complete his signature?"
        do
          p_ do
            "Bob's pre-signature was intentionally constructed as incomplete—\"adapted\" to Alice's commitment "
            term "code" [] "Y = y·B"
            ". The adapter signature mechanism ensures that "
            term "code" [] "y"
            " is the missing piece needed to complete the signature. This is what creates the atomic link: Bob can only complete his signature after Alice reveals "
            term "code" [] "y"
            " by completing hers."
      renderQA
        sid
        "qa-swap-atomic"
        "Why is the swap atomic?"
        do
          p_ do
            "The swap is atomic because of the cryptographic linking through "
            term "code" [] "y"
            ": Alice cannot claim Bob's funds without revealing "
            term "code" [] "y"
            ", and once "
            term "code" [] "y"
            " is revealed, Bob can always complete his signature and claim Alice's funds. There's no scenario where one party succeeds and the other fails. The adapter signature mechanism enforces this at the mathematical level."
      renderQA
        sid
        "qa-bob-not-complete"
        "What if Bob doesn't complete this step?"
        do
          p_ do
            "If Bob doesn't complete his signature after extracting "
            term "code" [] "y"
            ", he simply doesn't claim Alice's funds—but this is Bob's choice and only harms himself. Alice has already claimed Bob's funds. From a protocol perspective, the atomic property holds: both parties had the opportunity to complete, and Alice did complete. Bob's failure to act is not a protocol failure."

-- | Render a Q&A item with collapsible answer
renderQA :: Text -> Text -> Text -> Html () -> Html ()
renderQA stepId qaId question answer =
  let uniqueId = stepId <> "-" <> qaId
   in div_ [class_ "qa-item"] do
        term "input" [term "type" "checkbox", id_ uniqueId, class_ "qa-toggle"] ""
        term "label" [term "for" uniqueId, class_ "qa-question"] do
          span_ [class_ "qa-chevron"] "▸"
          toHtml question
        div_ [class_ "qa-answer"] answer

-- | Render explanation panel with collapsible content
renderExplanationPanel :: Text -> Html () -> Bool -> Html ()
renderExplanationPanel stepId content isOpenByDefault =
  let toggleId = "explain-" <> stepId
      checkedAttr = [term "checked" "" | isOpenByDefault]
   in div_ [class_ "explanation-panel"] do
        term
          "input"
          ( [term "type" "checkbox", id_ toggleId, class_ "explanation-toggle"]
              <> checkedAttr
          )
          ""
        term "label" [term "for" toggleId, class_ "explanation-trigger"] "Explanation"
        div_ [class_ "explanation-content"] do
          content
          term "label" [term "for" toggleId, class_ "explanation-hide"] "Hide explanation"

--------------------------------------------------------------------------------
-- Initial Page Template -------------------------------------------------------

-- | Main page with 3-column layout (renders current state)
mainPage :: SimulatorState -> Html ()
mainPage simState =
  let aliceState = getPartyState Alice simState
      bobState = getPartyState Bob simState
   in doctypehtml_ do
        head_ do
          meta_ [charset_ "utf-8"]
          meta_
            [ name_ "viewport"
            , content_ "width=device-width, initial-scale=1"
            ]
          title_ "Atomic Swap Simulator"
          -- HTMX
          script_ [src_ "https://unpkg.com/htmx.org@1.9.10"] ("" :: Text)
          -- CSS stylesheet
          link_ [rel_ "stylesheet", href_ "/static/style.css"]

        body_ do
          -- JavaScript for select-all on click for readonly inputs
          script_ $
            unlines
              [ "document.addEventListener('click', function(e) {"
              , "  if (e.target.tagName === 'INPUT' && e.target.hasAttribute('readonly') && e.target.value) {"
              , "    e.target.select();"
              , "  }"
              , "});"
              ]

          div_ [class_ "container"] do
            header_ [class_ "header"] do
              h1_ "Atomic Swap Simulator"
              p_
                "Step-by-step simulator of atomic swap protocol based on adapter signatures and non-interactive ZK proofs"

            div_ [class_ "main-content"] do
              -- Left column: Alice's state
              div_ [class_ "column alice-column"] do
                h2_ "Alice"

                -- Alice's available actions
                div_ [id_ "alice-actions", class_ "actions-section"] do
                  section_ "Actions"
                  renderAliceActionsFields simState

                div_ [id_ "alice-state", class_ "state-section"] do
                  section_ "State"
                  renderAliceStateFields aliceState

              -- Center column: Event timeline
              div_ [class_ "column timeline-column"] do
                div_ [class_ "timeline-header"] do
                  h2_ "Timeline"
                div_ [id_ "timeline", class_ "timeline"] do
                  renderNextActionHint simState
                  div_ [id_ "timeline-entries"] do
                    renderTimeline (ssSwapAmounts simState) (ssGlobalState simState)

              -- Right column: Bob's state
              div_ [class_ "column bob-column"] do
                h2_ "Bob"

                -- Bob's available actions
                div_ [id_ "bob-actions", class_ "actions-section"] do
                  section_ "Actions"
                  renderBobActionsFields simState

                div_ [id_ "bob-state", class_ "state-section"] do
                  section_ "State"
                  renderBobStateFields bobState

--------------------------------------------------------------------------------
-- Timeline Rendering ----------------------------------------------------------

-- | Render the complete execution timeline from history
renderTimeline :: (Quantity 'Apple, Quantity 'Banana) -> GlobalState -> Html ()
renderTimeline (Quantity apples, Quantity bananas) globalState =
  case reverse globalState of
    [] -> renderStep0 apples bananas
    (lastStep : olderSteps) -> do
      -- Render last step (tip) WITHOUT reset button
      renderTimelineEntryWithoutReset lastStep
      -- Render older steps WITH reset buttons
      mapM_ renderTimelineEntry olderSteps
      -- Render Step 0 at bottom with reset button
      renderStep0 apples bananas

-- | Render step 0 (initial agreement) with reset button
renderStep0 :: Natural -> Natural -> Html ()
renderStep0 apples bananas =
  div_ [class_ "timeline-item"] do
    span_ [class_ "step-description"] $
      toHtml
        ( "Alice and Bob agree to exchange "
            <> show apples
            <> " 🍎 for "
            <> show bananas
            <> " 🍌"
            :: Text
        )
    -- Reset all button (visible on hover, resets entire protocol)
    button_
      [ class_ "reset-to-step-button"
      , hxPost_ "/reset"
      , hxTarget_ "body"
      , hxSwap_ "outerHTML"
      , title_ "Reset entire protocol"
      ]
      do img_ [src_ "/static/icons/refresh.svg"]
    span_ [class_ "step-number"] "0"

-- | Render hint for next recommended action (with optional OOB for updates)
renderNextActionHint :: SimulatorState -> Html ()
renderNextActionHint = renderNextActionHintWithOob False

renderNextActionHintWithOob :: Bool -> SimulatorState -> Html ()
renderNextActionHintWithOob useOob simState =
  case recommendedAction simState of
    Nothing ->
      div_
        ( [id_ "next-action-hint", class_ "next-action-hint complete"]
            <> [hxSwapOob_ "true" | useOob]
        )
        "✓ Protocol complete!"
    Just action ->
      let metadata = actionMetadata action
          participantName = case amParticipant metadata of
            Alice -> "Alice" :: Text
            Bob -> "Bob"
          buttonClass = case amParticipant metadata of
            Alice -> "party-button alice-button" :: Text
            Bob -> "party-button bob-button"
       in div_
            ( [id_ "next-action-hint", class_ "next-action-hint"]
                <> [hxSwapOob_ "true" | useOob]
            )
            do
              span_ [class_ "badge"] "Recommended"
              span_ [style_ "margin-right: 8px;"] $
                toHtml ("Next step: " <> participantName <> " should")
              button_
                [ class_ (buttonClass <> " next-action-button")
                , hxPost_ (amEndpoint metadata)
                , hxTarget_ "#timeline-entries"
                , hxSwap_ "afterbegin"
                ]
                (toHtml $ amButtonText metadata)

-- | Render a single timeline entry (with reset button)
renderTimelineEntry :: StepEntry -> Html ()
renderTimelineEntry entry@(StepIndex idx, _, _, _) =
  div_ [id_ ("step-" <> show idx)] do
    renderTimelineEntryContent entry True

-- | Render a single timeline entry WITHOUT reset button (for tip)
renderTimelineEntryWithoutReset :: StepEntry -> Html ()
renderTimelineEntryWithoutReset entry@(StepIndex idx, _, _, _) =
  div_ [id_ ("step-" <> show idx)] do
    renderTimelineEntryContent entry False

-- | Render timeline entry content (with or without reset button)
renderTimelineEntryContent :: StepEntry -> Bool -> Html ()
renderTimelineEntryContent (StepIndex idx, participant, _inputs, updates) includeReset =
  let eventClass = case participant of
        Alice -> "timeline-item alice-event" :: Text
        Bob -> "timeline-item bob-event" :: Text
      description = describeUpdates participant updates
      iconName = getStepIcon description
      stepId = show idx
      maybeExplanation = getStepExplanation stepId description
      -- Tip (no reset button) should have explanation open by default
      isOpenByDefault = not includeReset
   in div_ [class_ eventClass] do
        span_ [class_ "step-description"] do
          stepIcon_ iconName
          toHtml description
        -- Explanation panel (if available)
        for_ maybeExplanation $ \explanation ->
          renderExplanationPanel stepId explanation isOpenByDefault
        -- Reset button (conditionally rendered)
        when includeReset $
          button_
            [ class_ "reset-to-step-button"
            , hxPost_ ("/reset-to-step/" <> show idx)
            , hxTarget_ "body"
            , hxSwap_ "outerHTML"
            , title_ "Reset to this step"
            ]
            do img_ [src_ "/static/icons/refresh.svg"]
        span_ [class_ "step-number"] $ show (idx + 1)

-- | Generate human-readable description from state updates
describeUpdates :: Participant -> [StateUpdate] -> Text
describeUpdates participant updates =
  case updates of
    [SetPrivateKey _ _, SetPublicKey _ _] ->
      case participant of
        Alice -> "Alice generated Ed25519 keypair"
        Bob -> "Bob generated Ed25519 keypair"
    [SetOtherPartyPublicKey Bob _, SetSentPublicKey Alice _] ->
      "Alice shared her public key with Bob"
    [SetOtherPartyPublicKey Alice _, SetSentPublicKey Bob _] ->
      "Bob shared his public key with Alice"
    [SetAdapterSecret _ _] ->
      "Alice generated adapter secret y"
    [SetAdapterCommitment _ _] ->
      "Alice computed commitment Y = y·B"
    [SetOtherPartyCommitment Bob _, SetSentCommitment Alice _] ->
      "Alice shared commitment Y with Bob"
    [SetNIZKProof _ _] ->
      "Alice generated seal proof for commitment"
    [SetOtherPartyNIZKProof Bob _, SetSentNIZKProof Alice _] ->
      "Alice shared seal proof with Bob"
    [SetNIZKProofVerified Bob _] ->
      "Bob verified Alice's seal proof successfully"
    [SetTransaction Alice _] ->
      "Alice prepared transaction (apples → Bob)"
    [SetPreSignature Alice _] ->
      "Alice created adapter pre-signature"
    [ SetSentPreSignature Alice _
      , SetOtherPartyTransaction Bob _
      , SetOtherPartyPreSignature Bob _
      ] ->
        "Alice published transaction and pre-signature to Bob"
    [SetPreSignatureVerified Bob _] ->
      "Bob verified Alice's pre-signature successfully"
    [SetTransaction Bob _] ->
      "Bob prepared transaction (bananas → Alice)"
    [SetPreSignature Bob _] ->
      "Bob created adapter pre-signature"
    [ SetSentPreSignature Bob _
      , SetOtherPartyTransaction Alice _
      , SetOtherPartyPreSignature Alice _
      ] ->
        "Bob published transaction and pre-signature to Alice"
    [SetPreSignatureVerified Alice _] ->
      "Alice verified Bob's pre-signature successfully"
    [SetCompleteSignature Alice _, SetOtherPartyCompleteSignature Bob _] ->
      "Alice completed signature and published to blockchain (reveals secret y!)"
    [SetExtractedSecret Bob _] ->
      "Bob extracted adapter secret y from Alice's signature"
    [SetCompleteSignature Bob _] ->
      "Bob completed signature and published to blockchain"
    _ -> case participant of
      Alice -> "Alice performed action"
      Bob -> "Bob performed action"

--------------------------------------------------------------------------------
-- HTMX Helpers ----------------------------------------------------------------

-- | HTMX post attribute
hxPost_ :: Text -> Attributes
hxPost_ url = term "hx-post" url

-- | HTMX target attribute
hxTarget_ :: Text -> Attributes
hxTarget_ target = term "hx-target" target

-- | HTMX swap attribute
hxSwap_ :: Text -> Attributes
hxSwap_ mode = term "hx-swap" mode

-- | HTMX out-of-band swap attribute
hxSwapOob_ :: Text -> Attributes
hxSwapOob_ value = term "hx-swap-oob" value

--------------------------------------------------------------------------------
-- State-Diffing Renderers -----------------------------------------------------

{- | Render state updates for changed parties (state-diffing approach)

Takes list of changed participants and renders their state/action panels
via HTMX out-of-band swaps. Used after detecting which parties changed.

IMPORTANT: Action buttons are ALWAYS updated for BOTH parties, even if only one
party's state changed, because the recommended action is global and can change
when the other party acts.
-}
renderStateUpdates :: [Participant] -> SimulatorState -> Html ()
renderStateUpdates changedParties simState =
  let aliceState = getPartyState Alice simState
      bobState = getPartyState Bob simState
   in do
        -- Update state panels only for changed parties
        forM_ changedParties \case
          Alice -> renderAliceStateUpdate aliceState
          Bob -> renderBobStateUpdate bobState
        -- ALWAYS update action buttons for BOTH parties (recommendation can change)
        renderAliceActionsUpdate simState
        renderBobActionsUpdate simState

{- | Render new timeline entry from the latest step in GlobalState

Reads the last entry from GlobalState to create timeline event.
The new entry is the tip (no reset button), and we need to update
the previous tip to add its reset button via OOB swap.
-}
renderNewTimelineEntry :: SimulatorState -> Html ()
renderNewTimelineEntry simState = do
  let globalState = ssGlobalState simState
  -- Render new tip WITHOUT reset button
  for_ (viaNonEmpty last globalState) renderTimelineEntryWithoutReset
  -- Update previous tip to ADD reset button (if it exists)
  -- Get second-to-last element (previous tip)
  case viaNonEmpty init globalState of
    Just olderSteps -> for_ (viaNonEmpty last olderSteps) renderPreviousTipUpdate
    Nothing -> pure () -- Only one step, no previous tip
    -- Update hint with OOB swap (no need to delete first, OOB replaces it)
  renderNextActionHintWithOob True simState

-- | Render OOB update for previous tip (to add reset button)
renderPreviousTipUpdate :: StepEntry -> Html ()
renderPreviousTipUpdate entry@(StepIndex idx, _, _, _) =
  div_
    [ hxSwapOob_ ("outerHTML:#step-" <> show idx)
    , id_ ("step-" <> show idx)
    ]
    do renderTimelineEntryContent entry True

--------------------------------------------------------------------------------
-- State Rendering -------------------------------------------------------------

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
renderAliceStateUpdate :: PartyState -> Html ()
renderAliceStateUpdate partyState = do
  div_ [hxSwapOob_ "innerHTML:#alice-state"] do
    section_ "State"
    renderAliceStateFields partyState

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
renderBobStateUpdate :: PartyState -> Html ()
renderBobStateUpdate partyState = do
  div_ [hxSwapOob_ "innerHTML:#bob-state"] do
    section_ "State"
    renderBobStateFields partyState

--------------------------------------------------------------------------------
-- Action Rendering ------------------------------------------------------------

-- | Render Alice's action buttons (no OOB wrapper)
renderAliceActionsFields :: SimulatorState -> Html ()
renderAliceActionsFields simState =
  let aliceActions = filter isAliceAction (availableActions simState)
      hasActions = not (null aliceActions)
      recommended = recommendedAction simState
   in if hasActions
        then
          div_ [class_ "button-group"] $
            mapM_ (renderActionButton recommended) aliceActions
        else div_ [class_ "no-actions"] "Waiting for Bob..."
  where
    isAliceAction :: Action -> Bool
    isAliceAction action =
      let metadata = actionMetadata action
       in amParticipant metadata == Alice

    renderActionButton :: Maybe Action -> Action -> Html ()
    renderActionButton recommended action =
      let metadata = actionMetadata action
          isRecommended = Just action == recommended
          buttonClass =
            if isRecommended
              then "party-button alice-button"
              else "party-button alice-button non-recommended"
       in button_
            [ class_ buttonClass
            , hxPost_ (amEndpoint metadata)
            , hxTarget_ "#timeline-entries"
            , hxSwap_ "afterbegin"
            ]
            (toHtml $ amButtonText metadata)

-- | Render Alice's actions with OOB wrapper
renderAliceActionsUpdate :: SimulatorState -> Html ()
renderAliceActionsUpdate simState =
  div_ [hxSwapOob_ "innerHTML:#alice-actions"] do
    section_ "Actions"
    renderAliceActionsFields simState

-- | Render Bob's action buttons (no OOB wrapper)
renderBobActionsFields :: SimulatorState -> Html ()
renderBobActionsFields simState =
  let bobActions = filter isBobAction (availableActions simState)
      hasActions = not (null bobActions)
      recommended = recommendedAction simState
   in if hasActions
        then
          div_ [class_ "button-group"] $
            mapM_ (renderActionButton recommended) bobActions
        else div_ [class_ "no-actions"] "Waiting for Alice..."
  where
    isBobAction :: Action -> Bool
    isBobAction action =
      let metadata = actionMetadata action
       in amParticipant metadata == Bob

    renderActionButton :: Maybe Action -> Action -> Html ()
    renderActionButton recommended action =
      let metadata = actionMetadata action
          isRecommended = Just action == recommended
          buttonClass =
            if isRecommended
              then "party-button bob-button"
              else "party-button bob-button non-recommended"
       in button_
            [ class_ buttonClass
            , hxPost_ (amEndpoint metadata)
            , hxTarget_ "#timeline-entries"
            , hxSwap_ "afterbegin"
            ]
            (toHtml $ amButtonText metadata)

-- | Render Bob's actions with OOB wrapper
renderBobActionsUpdate :: SimulatorState -> Html ()
renderBobActionsUpdate simState = do
  div_ [hxSwapOob_ "innerHTML:#bob-actions"] do
    section_ "Actions"
    renderBobActionsFields simState

--------------------------------------------------------------------------------
-- Helper Functions ------------------------------------------------------------

-- | Render a readonly input field for state values (allows full text selection)
stateValueInput_ :: Text -> Html ()
stateValueInput_ value =
  term
    "input"
    [ class_ "state-value"
    , term "type" "text"
    , term "readonly" ""
    , term "value" value
    ]
    ""

-- | Format bytes as hex string
formatHex :: ByteString -> Text
formatHex = TE.decodeUtf8 . Base16.encode
