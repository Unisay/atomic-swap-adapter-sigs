{-# LANGUAGE StrictData #-}

{- |
Module: AtomicSwap.Simulator.Render.Explanations
Description: Educational explanations for protocol steps

Provides comprehensive Q&A explanations for each step in the atomic swap
protocol. Each explanation includes introductory text and collapsible
question-answer sections covering cryptographic concepts, security properties,
and protocol mechanics.
-}
module AtomicSwap.Simulator.Render.Explanations
  ( -- * Explanation Rendering
    getStepExplanation
  , renderQA
  , renderExplanationPanel
  ) where

import Data.Text qualified as T
import Lucid
  ( Html
  , class_
  , div_
  , id_
  , p_
  , span_
  , strong_
  , term
  , toHtml
  )
import Prelude

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
