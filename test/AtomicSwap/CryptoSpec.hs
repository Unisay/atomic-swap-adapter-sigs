{- |
Module: AtomicSwap.CryptoSpec
Description: Unit tests for cryptographic primitives

Comprehensive test coverage for:
- Ed25519 key generation
- rEdDSA signatures
- Adapter signatures
- NIZK discrete log proofs
-}
module AtomicSwap.CryptoSpec (spec) where

import Test.Hspec

import AtomicSwap.Crypto.Adapter
  ( adaptSignature
  , extractAdapterSecret
  , generateAdapterCommitment
  , generateAdapterSecret
  , preSignREdDSA
  , preVerifyREdDSA
  )
import AtomicSwap.Crypto.Keys (generateKeyPair)
import AtomicSwap.Crypto.NIZK (proveDiscreteLog, verifyDiscreteLog)
import AtomicSwap.Crypto.Signatures (signREdDSA, verifyREdDSA)

spec :: Spec
spec = do
  describe "Ed25519 Key Generation" do
    it "generates valid keypair" do
      (privKey, pubKey) <- generateKeyPair
      -- Just verify it doesn't crash and returns non-empty values
      show privKey `shouldNotBe` ("" :: String)
      show pubKey `shouldNotBe` ("" :: String)

    it "generates different keypairs on each call" do
      (_, pubKey1) <- generateKeyPair
      (_, pubKey2) <- generateKeyPair
      pubKey1 `shouldNotBe` pubKey2

  describe "rEdDSA Signatures" do
    it "signs and verifies a message correctly" do
      (privKey, pubKey) <- generateKeyPair
      let message = "Hello, atomic swaps!"

      sig <- signREdDSA privKey pubKey message
      let result = verifyREdDSA pubKey message sig

      result `shouldBe` True

    it "rejects signature for different message" do
      (privKey, pubKey) <- generateKeyPair
      let message1 = "Original message"
      let message2 = "Different message"

      sig <- signREdDSA privKey pubKey message1
      let result = verifyREdDSA pubKey message2 sig

      result `shouldBe` False

    it "rejects signature from different key" do
      (privKey1, _) <- generateKeyPair
      (_, pubKey2) <- generateKeyPair
      let message = "Test message"

      sig <- signREdDSA privKey1 pubKey2 message -- Sign with key1
      let result = verifyREdDSA pubKey2 message sig -- Verify with key2
      result `shouldBe` False

    it "produces different signatures for same message (randomized)" do
      (privKey, pubKey) <- generateKeyPair
      let message = "Same message"

      sig1 <- signREdDSA privKey pubKey message
      sig2 <- signREdDSA privKey pubKey message

      -- Signatures should be different (due to randomness k)
      sig1 `shouldNotBe` sig2

      -- But both should verify
      verifyREdDSA pubKey message sig1 `shouldBe` True
      verifyREdDSA pubKey message sig2 `shouldBe` True

  describe "Adapter Signatures" do
    it "creates and verifies adapted pre-signature" do
      (privKey, pubKey) <- generateKeyPair
      secret <- generateAdapterSecret
      let commitment = generateAdapterCommitment secret
      proof <- proveDiscreteLog secret commitment

      let message = "Transaction data"
      preSig <- preSignREdDSA privKey pubKey message commitment proof

      let result = preVerifyREdDSA pubKey message commitment preSig proof
      result `shouldBe` True

    it "completes adapted signature correctly" do
      (privKey, pubKey) <- generateKeyPair
      secret <- generateAdapterSecret
      let commitment = generateAdapterCommitment secret
      proof <- proveDiscreteLog secret commitment

      let message = "Transaction data"
      preSig <- preSignREdDSA privKey pubKey message commitment proof

      -- Complete the signature
      let completeSig = adaptSignature preSig secret

      -- The complete signature should verify as a regular rEdDSA signature
      let result = verifyREdDSA pubKey message completeSig
      result `shouldBe` True

    it "extracts adapter secret correctly (THE KEY PROPERTY!)" do
      (privKey, pubKey) <- generateKeyPair
      secret <- generateAdapterSecret
      let commitment = generateAdapterCommitment secret
      proof <- proveDiscreteLog secret commitment

      let message = "Transaction data"
      preSig <- preSignREdDSA privKey pubKey message commitment proof

      -- Complete the signature
      let completeSig = adaptSignature preSig secret

      -- Extract the secret from pre-signature and complete signature
      let extracted = extractAdapterSecret preSig completeSig

      -- The extracted secret should match the original!
      extracted `shouldBe` secret

    it "demonstrates atomic swap property" do
      -- This test shows how adapter signatures enable atomic swaps

      -- Alice generates adapter secret (this links both transactions)
      aliceSecret <- generateAdapterSecret
      let aliceCommitment = generateAdapterCommitment aliceSecret
      aliceProof <- proveDiscreteLog aliceSecret aliceCommitment

      -- Alice creates her keypair and pre-signature
      (alicePrivKey, alicePubKey) <- generateKeyPair
      let aliceMessage = "Alice's transaction on ChainA"
      alicePreSig <-
        preSignREdDSA alicePrivKey alicePubKey aliceMessage aliceCommitment aliceProof

      -- Bob creates his keypair and pre-signature using Alice's commitment!
      (bobPrivKey, bobPubKey) <- generateKeyPair
      let bobMessage = "Bob's transaction on ChainB"
      bobPreSig <-
        preSignREdDSA bobPrivKey bobPubKey bobMessage aliceCommitment aliceProof

      -- Both verify each other's pre-signatures
      preVerifyREdDSA alicePubKey aliceMessage aliceCommitment alicePreSig aliceProof
        `shouldBe` True
      preVerifyREdDSA bobPubKey bobMessage aliceCommitment bobPreSig aliceProof
        `shouldBe` True

      -- Alice publishes her complete signature (reveals secret)
      let aliceCompleteSig = adaptSignature alicePreSig aliceSecret
      verifyREdDSA alicePubKey aliceMessage aliceCompleteSig `shouldBe` True

      -- Bob extracts the secret from Alice's published signature
      let extractedSecret = extractAdapterSecret alicePreSig aliceCompleteSig
      extractedSecret `shouldBe` aliceSecret

      -- Bob uses extracted secret to complete his signature
      let bobCompleteSig = adaptSignature bobPreSig extractedSecret
      verifyREdDSA bobPubKey bobMessage bobCompleteSig `shouldBe` True

  -- ATOMICITY: Alice couldn't claim without revealing secret,
  -- and Bob could extract secret to claim his funds!

  describe "NIZK Discrete Log Proofs" do
    it "generates and verifies proof correctly" do
      secret <- generateAdapterSecret
      let commitment = generateAdapterCommitment secret

      proof <- proveDiscreteLog secret commitment
      let result = verifyDiscreteLog commitment proof

      result `shouldBe` True

    it "rejects proof for wrong commitment" do
      secret1 <- generateAdapterSecret
      secret2 <- generateAdapterSecret
      let commitment1 = generateAdapterCommitment secret1
      let commitment2 = generateAdapterCommitment secret2

      proof <- proveDiscreteLog secret1 commitment1
      let result = verifyDiscreteLog commitment2 proof -- Wrong commitment!
      result `shouldBe` False
