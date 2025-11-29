{- |
Module: AtomicSwap.HappyPathSpec
Description: Test specification for successful atomic swap

This module contains the HSpec test for the happy path scenario where
Alice and Bob successfully complete an atomic swap using adapter signatures.

The test demonstrates:
1. Alice and Bob exchange keys and adapter commitments
2. Both create adapted pre-signatures for their transactions
3. Alice publishes first, revealing the adapter secret
4. Bob extracts the adapter secret and completes his transaction
5. Both parties successfully receive funds on the opposite chain
-}
module AtomicSwap.HappyPathSpec (spec) where

import Control.Concurrent.Async (async, waitBoth)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe)

import AtomicSwap.Blockchain.Ledger (getBalance, initBlockchain)
import AtomicSwap.Blockchain.Types (GenesisUTXO (..))
import AtomicSwap.Crypto.Keys (generateKeyPair)
import AtomicSwap.Protocol.Alice (aliceProtocol)
import AtomicSwap.Protocol.Bob (bobProtocol)
import AtomicSwap.Protocol.Messaging (newMessageQueue)
import AtomicSwap.Types (Participant (..), Party (..), SwapResult (..))

--------------------------------------------------------------------------------
-- Test Suite ------------------------------------------------------------------

spec :: Spec
spec = describe "Atomic Swap - Happy Path" do
  it "completes successful atomic swap between Alice and Bob" do
    withSystemTempDirectory "atomic-swap-test" \tmpDir -> do
      -- Setup: Generate keys for Alice and Bob
      (aliceSk, alicePk) <- generateKeyPair
      (bobSk, bobPk) <- generateKeyPair

      let
        -- Create parties
        alice = Party Alice aliceSk alicePk
        bob = Party Bob bobSk bobPk

      -- Initialize blockchains with genesis UTXOs
      -- Alice has 10 tokens on ChainA, Bob has 5 tokens on ChainB
      let chainAPath = tmpDir <> "/chain-a.json"
          chainBPath = tmpDir <> "/chain-b.json"
          genesisA = [GenesisUTXO alicePk 10]
          genesisB = [GenesisUTXO bobPk 5]

      chainA <- initBlockchain "ChainA" chainAPath genesisA
      chainB <- initBlockchain "ChainB" chainBPath genesisB

      -- Create message queues for communication
      aliceQueue <- newMessageQueue -- Alice receives on this queue
      bobQueue <- newMessageQueue -- Bob receives on this queue

      -- Initial balances
      aliceInitialA <- getBalance chainA alicePk
      aliceInitialA `shouldBe` 10

      bobInitialB <- getBalance chainB bobPk
      bobInitialB `shouldBe` 5

      -- Run both protocols concurrently
      -- Alice sends 10 tokens to Bob on ChainA
      -- Bob sends 5 tokens to Alice on ChainB
      aliceAsync <- async $ aliceProtocol alice chainA aliceQueue bobQueue 10
      bobAsync <- async $ bobProtocol bob chainB bobQueue aliceQueue 5
      (aliceResult, bobResult) <- waitBoth aliceAsync bobAsync

      -- Verify both protocols succeeded
      aliceResult `shouldBe` SwapSuccess
      bobResult `shouldBe` SwapSuccess

      -- Verify final balances
      -- Alice should now have 0 on ChainA (sent all 10 to Bob)
      aliceFinalA <- getBalance chainA alicePk
      aliceFinalA `shouldBe` 0

      -- Alice should now have 5 on ChainB (received from Bob)
      aliceFinalB <- getBalance chainB alicePk
      aliceFinalB `shouldBe` 5

      -- Bob should now have 10 on ChainA (received from Alice)
      bobFinalA <- getBalance chainA bobPk
      bobFinalA `shouldBe` 10

      -- Bob should now have 0 on ChainB (sent all 5 to Alice)
      bobFinalB <- getBalance chainB bobPk
      bobFinalB `shouldBe` 0
