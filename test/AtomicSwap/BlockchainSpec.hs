{- |
Module: AtomicSwap.BlockchainSpec
Description: Tests for blockchain simulation layer

Comprehensive tests for:
- Blockchain initialization and genesis UTXOs
- Transaction construction and hashing
- Transaction verification (signatures, double-spending, conservation)
- UTXO queries and balance calculations
- JSON persistence and loading
-}
module AtomicSwap.BlockchainSpec (spec) where

import Data.List (isInfixOf)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import AtomicSwap.Blockchain.Ledger
  ( getBalance
  , initBlockchain
  , loadBlockchain
  , queryUTXOs
  , saveBlockchain
  , submitTransaction
  )
import AtomicSwap.Blockchain.Transaction (buildTransaction, hashTransaction)
import AtomicSwap.Blockchain.Types (GenesisUTXO (..))
import AtomicSwap.Crypto.Keys (generateKeyPair)
import AtomicSwap.Crypto.Signatures (signREdDSA)
import AtomicSwap.Types (Output (..), Transaction (..), TxId (..), UTXO (..))

--------------------------------------------------------------------------------
-- Test Suite ------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Blockchain Initialization" do
    it "creates blockchain with genesis UTXOs" do
      withSystemTempDirectory "blockchain-test" \tmpDir -> do
        (_, alicePk) <- generateKeyPair
        (_, bobPk) <- generateKeyPair

        let genesisUTXOs =
              [ GenesisUTXO alicePk 10
              , GenesisUTXO bobPk 5
              ]

        blockchain <- initBlockchain "ChainA" (tmpDir <> "/chain-a.json") genesisUTXOs

        -- Check Alice's balance
        aliceBalance <- getBalance blockchain alicePk
        aliceBalance `shouldBe` 10

        -- Check Bob's balance
        bobBalance <- getBalance blockchain bobPk
        bobBalance `shouldBe` 5

    it "persists and loads blockchain state" do
      withSystemTempDirectory "blockchain-test" \tmpDir -> do
        (_, alicePk) <- generateKeyPair

        let path = tmpDir <> "/chain-persist.json"
            genesisUTXOs = [GenesisUTXO alicePk 100]

        -- Create and save
        blockchain1 <- initBlockchain "ChainA" path genesisUTXOs
        saveBlockchain blockchain1

        -- Load and verify
        loadResult <- loadBlockchain "ChainA" path
        case loadResult of
          Left err -> fail $ "Failed to load blockchain: " <> toString err
          Right blockchain2 -> do
            balance <- getBalance blockchain2 alicePk
            balance `shouldBe` 100

  describe "Transaction Construction" do
    it "builds transaction with inputs and outputs" do
      (_, alicePk) <- generateKeyPair
      (_, bobPk) <- generateKeyPair

      let genesisTxId = TxId (fromString "0000")
          input =
            UTXO
              { utxoTxId = genesisTxId
              , utxoIndex = 0
              , utxoAmount = 10
              , utxoOwner = alicePk
              }
          output = Output bobPk 10

      let tx = buildTransaction [input] [output]

      txInputs tx `shouldBe` [input]
      txOutputs tx `shouldBe` [output]
      txSignatures tx `shouldBe` []

    it "produces consistent transaction hashes" do
      (_, alicePk) <- generateKeyPair
      (_, bobPk) <- generateKeyPair

      let genesisTxId = TxId (fromString "0000")
          input = UTXO genesisTxId 0 10 alicePk
          output = Output bobPk 10
          tx = buildTransaction [input] [output]

      let hash1 = hashTransaction tx
          hash2 = hashTransaction tx

      hash1 `shouldBe` hash2

  describe "Transaction Verification" do
    it "accepts valid transaction with correct signature" do
      withSystemTempDirectory "blockchain-test" \tmpDir -> do
        (aliceSk, alicePk) <- generateKeyPair
        (_, bobPk) <- generateKeyPair

        -- Initialize blockchain with Alice's funds
        let genesisUTXOs = [GenesisUTXO alicePk 10]
        blockchain <- initBlockchain "ChainA" (tmpDir <> "/chain.json") genesisUTXOs

        -- Query Alice's UTXO
        aliceUTXOs <- queryUTXOs blockchain alicePk
        aliceUTXOs `shouldSatisfy` (not . null)

        input <- case viaNonEmpty head aliceUTXOs of
          Just utxo -> pure utxo
          Nothing -> fail "Expected at least one UTXO for Alice"
        let output = Output bobPk 10

        -- Build and sign transaction
        let tx = buildTransaction [input] [output]
            txHash = hashTransaction tx

        sig <- signREdDSA aliceSk alicePk txHash
        let signedTx = tx {txSignatures = [sig]}

        -- Submit transaction
        result <- submitTransaction blockchain signedTx
        case result of
          Left err -> fail $ "Transaction rejected: " <> toString err
          Right _txId -> do
            -- Verify balances updated
            aliceBalance <- getBalance blockchain alicePk
            bobBalance <- getBalance blockchain bobPk
            aliceBalance `shouldBe` 0
            bobBalance `shouldBe` 10

    it "rejects transaction with insufficient funds" do
      withSystemTempDirectory "blockchain-test" \tmpDir -> do
        (aliceSk, alicePk) <- generateKeyPair
        (_, bobPk) <- generateKeyPair

        -- Initialize blockchain with Alice having 5 tokens
        let genesisUTXOs = [GenesisUTXO alicePk 5]
        blockchain <- initBlockchain "ChainA" (tmpDir <> "/chain.json") genesisUTXOs

        -- Try to send 10 tokens (more than she has)
        aliceUTXOs <- queryUTXOs blockchain alicePk
        input <- case viaNonEmpty head aliceUTXOs of
          Just utxo -> pure utxo
          Nothing -> fail "Expected at least one UTXO for Alice"
        let output = Output bobPk 10 -- More than input!
        let tx = buildTransaction [input] [output]
            txHash = hashTransaction tx

        sig <- signREdDSA aliceSk alicePk txHash
        let signedTx = tx {txSignatures = [sig]}

        result <- submitTransaction blockchain signedTx
        case result of
          Left err -> err `shouldSatisfy` (\e -> "exceeds" `isInfixOf` toString e)
          Right _ -> fail "Should have rejected transaction with insufficient funds"

    it "rejects transaction with invalid signature" do
      withSystemTempDirectory "blockchain-test" \tmpDir -> do
        (_aliceSk, alicePk) <- generateKeyPair
        (bobSk, bobPk) <- generateKeyPair

        let genesisUTXOs = [GenesisUTXO alicePk 10]
        blockchain <- initBlockchain "ChainA" (tmpDir <> "/chain.json") genesisUTXOs

        aliceUTXOs <- queryUTXOs blockchain alicePk
        input <- case viaNonEmpty head aliceUTXOs of
          Just utxo -> pure utxo
          Nothing -> fail "Expected at least one UTXO for Alice"
        let output = Output bobPk 10

        let tx = buildTransaction [input] [output]
            txHash = hashTransaction tx

        -- Sign with WRONG key (Bob instead of Alice)
        wrongSig <- signREdDSA bobSk bobPk txHash
        let signedTx = tx {txSignatures = [wrongSig]}

        result <- submitTransaction blockchain signedTx
        case result of
          Left err -> err `shouldSatisfy` (\e -> "Invalid signature" `isInfixOf` toString e)
          Right _ -> fail "Should have rejected transaction with invalid signature"

    it "prevents double-spending" do
      withSystemTempDirectory "blockchain-test" \tmpDir -> do
        (aliceSk, alicePk) <- generateKeyPair
        (_, bobPk) <- generateKeyPair

        let genesisUTXOs = [GenesisUTXO alicePk 10]
        blockchain <- initBlockchain "ChainA" (tmpDir <> "/chain.json") genesisUTXOs

        aliceUTXOs <- queryUTXOs blockchain alicePk
        input <- case viaNonEmpty head aliceUTXOs of
          Just utxo -> pure utxo
          Nothing -> fail "Expected at least one UTXO for Alice"
        let output = Output bobPk 10

        let tx = buildTransaction [input] [output]
            txHash = hashTransaction tx

        sig <- signREdDSA aliceSk alicePk txHash
        let signedTx = tx {txSignatures = [sig]}

        -- Submit first time - should succeed
        result1 <- submitTransaction blockchain signedTx
        case result1 of
          Left err -> fail $ "First submission failed: " <> toString err
          Right _ -> pure ()

        -- Try to submit same transaction again - should fail
        result2 <- submitTransaction blockchain signedTx
        case result2 of
          Left err -> err `shouldSatisfy` (\e -> "not found" `isInfixOf` toString e)
          Right _ -> fail "Should have prevented double-spending"

  describe "UTXO Queries" do
    it "queries UTXOs by public key" do
      withSystemTempDirectory "blockchain-test" \tmpDir -> do
        (_, alicePk) <- generateKeyPair
        (_, bobPk) <- generateKeyPair

        let genesisUTXOs = [GenesisUTXO alicePk 10, GenesisUTXO bobPk 5]
        blockchain <- initBlockchain "ChainA" (tmpDir <> "/chain.json") genesisUTXOs

        aliceUTXOs <- queryUTXOs blockchain alicePk
        bobUTXOs <- queryUTXOs blockchain bobPk

        length aliceUTXOs `shouldBe` 1
        length bobUTXOs `shouldBe` 1

        aliceUtxo <- case viaNonEmpty head aliceUTXOs of
          Just utxo -> pure utxo
          Nothing -> fail "Expected at least one UTXO for Alice"
        bobUtxo <- case viaNonEmpty head bobUTXOs of
          Just utxo -> pure utxo
          Nothing -> fail "Expected at least one UTXO for Bob"

        utxoAmount aliceUtxo `shouldBe` 10
        utxoAmount bobUtxo `shouldBe` 5

    it "calculates total balance correctly" do
      withSystemTempDirectory "blockchain-test" \tmpDir -> do
        (_, alicePk) <- generateKeyPair

        -- Give Alice multiple UTXOs
        let genesisUTXOs =
              [ GenesisUTXO alicePk 10
              , GenesisUTXO alicePk 20
              , GenesisUTXO alicePk 30
              ]
        blockchain <- initBlockchain "ChainA" (tmpDir <> "/chain.json") genesisUTXOs

        balance <- getBalance blockchain alicePk
        balance `shouldBe` 60
