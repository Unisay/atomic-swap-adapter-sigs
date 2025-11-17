---
Generated: 2025-11-14 21:30
Type: research
---

# Two-Party Atomic Swap Protocol Using Adapter Signatures

## Executive Summary

Adapter signatures enable trustless cross-chain atomic swaps between two parties (Alice and Bob) by cryptographically linking transactions on different blockchains. Unlike hash time-locked contracts (HTLCs), adapter signatures provide superior privacy, efficiency, and scriptless execution.

**Key Insight**: One party (the initiator, typically Alice) generates a secret `t` and its public commitment `T = tG`. Both parties create adapter signatures using this same `T` value, cryptographically linking their transactions. When Alice publishes her complete signature to claim funds, the secret `t` becomes extractable by Bob, who can then complete his signature and claim his funds. This creates atomicity: either both swaps complete or neither does.

**Critical Properties**:

- **Atomicity**: Both transactions complete or both fail
- **Privacy**: No on-chain linkage between swaps (unlike HTLC hash preimages)
- **Efficiency**: Reduced on-chain footprint (~67 bytes saved vs HTLC)
- **Scriptless**: Works with simple multisig, no complex scripts needed

**Fairness Caveat**: Standard atomic swaps give the initiator optionality (American call option without premium), allowing them to abort after seeing market conditions. This creates 2-3% unfairness for cryptocurrencies. Solutions include premium mechanisms or protocol modifications to eliminate unilateral decision points.

## Background: Adapter Signatures vs HTLCs

### Traditional HTLC Approach

Hash Time-Locked Contracts require:

- Hash preimage shared across both chains (linkable, privacy leak)
- On-chain hash publication (~67 bytes)
- Complex scripting support on both blockchains
- Visible linkage between transactions

### Adapter Signature Approach

Adapter signatures replace hash locks with signature-based secret revelation:

- Secret embedded in signature mathematics, not hash preimages
- No on-chain script publication (appears as normal signature)
- Works with simple multisig, minimal blockchain requirements
- Transactions appear unrelated to external observers

**Core Innovation**: Adapter signatures bind secret revelation to transaction authorization. Publishing a valid signature inherently reveals the adapter secret, enabling the counterparty to complete their transaction.

## Protocol Flow: Detailed Step-by-Step

### Phase 1: Setup and Key Generation

**Alice (Initiator)**:

1. Generates private key `a`, computes public key `A = aG`
2. Generates nonce `r_A`, computes `R_A = r_A·G`
3. **Generates adapter secret** `t`, computes `T = tG`
4. Shares `A`, `R_A`, and `T` with Bob

**Bob (Responder)**:

1. Generates private key `b`, computes public key `B = bG`
2. Generates nonce `r_B`, computes `R_B = r_B·G`
3. Shares `B` and `R_B` with Alice

**Result**: Both parties have public commitments but no complete signatures yet.

### Phase 2: Multisig Funding

Both parties lock funds in 2-of-2 multisignature addresses:

**Bitcoin Chain (Alice → Bob)**:

- Address requires signatures from both `A` and `B`
- Alice locks X BTC
- Refund timelock: `T_A` (e.g., 48 hours)

**Alternative Chain (Bob → Alice)**:

- Address requires signatures from both `A` and `B`
- Bob locks Y altcoins
- Refund timelock: `T_B` (e.g., 24 hours, shorter to prevent double-claim)

**Critical**: Refund timelocks must be properly staggered (`T_B < T_A`) to prevent optionality attacks.

### Phase 3: Adapter Signature Exchange

**Alice Creates Adapter Pre-Signature** (for Bob's transaction):

For Schnorr signatures:

1. Compute challenge: `c_A = H(R_A + T || A || m_A)` where `m_A` describes Bob's transaction
2. Create adapted signature: `s'_A = r_A + t + c_A·a`
3. Share `(s'_A, R_A, T)` with Bob

**Bob Verifies Alice's Adapter**:

```
Verification equation: s'_A·G = R_A + T + c_A·A
```

If verification passes, Alice's adapter is valid (but not yet a complete signature).

**Bob Creates Adapter Pre-Signature** (for Alice's transaction):

1. Compute challenge: `c_B = H(R_B + T || B || m_B)` where `m_B` describes Alice's transaction
2. Create adapted signature: `s'_B = r_B + t + c_B·b`

**Critical Insight**: Bob creates `s'_B` using the same `T` value Alice provided, even though Bob doesn't know `t`. This links both transactions cryptographically.

3. Share `(s'_B, R_B, T)` with Alice

**Alice Verifies Bob's Adapter**:

```
Verification equation: s'_B·G = R_B + T + c_B·B
```

**Result**: Both parties have verified adapter signatures from each other, but neither can complete their transaction yet.

### Phase 4: Secret Revelation and Atomic Execution

**Step 1: Alice Claims Bob's Funds**

Alice knows `t`, so she can complete her signature:

1. Compute complete signature: `s_A = s'_A - t = r_A + c_A·a`
2. Publish `(s_A, R_A)` on Bitcoin chain
3. Transaction valid because: `s_A·G = R_A + c_A·A` (standard Schnorr verification)
4. Alice receives Bob's X BTC

**Step 2: Bob Extracts Secret**

Bob observes Alice's published signature `(s_A, R_A)` on-chain:

1. Extract secret: `t = s'_A - s_A`
2. This works because:
   - Alice gave Bob: `s'_A = r_A + t + c_A·a`
   - Alice published: `s_A = r_A + c_A·a`
   - Therefore: `t = s'_A - s_A`

**Step 3: Bob Claims Alice's Funds**

Now Bob can complete his signature:

1. Compute complete signature: `s_B = s'_B - t = r_B + c_B·b`
2. Publish `(s_B, R_B)` on alternative chain
3. Transaction valid because: `s_B·G = R_B + c_B·B`
4. Bob receives Alice's Y altcoins

**Atomicity Achieved**: Alice cannot claim Bob's funds without revealing `t`, and once `t` is revealed, Bob can always claim Alice's funds.

## Cryptographic Linkage Mechanism

### How the Same Secret Links Both Transactions

The adapter secret `T = tG` appears in both adapter signatures:

**Alice's adapter**: `s'_A·G = R_A + T + c_A·A`
**Bob's adapter**: `s'_B·G = R_B + T + c_B·B`

Both equations contain the same `T` term. This creates a dependency:

1. **Before revelation**: Neither party can create a valid signature
   - Alice needs to remove `T` from `s'_A` to get valid `s_A`
   - Bob needs to remove `T` from `s'_B` to get valid `s_B`

2. **After Alice reveals**: Alice publishes `s_A` (which implicitly reveals `t`)
   - Bob computes: `t = s'_A - s_A`
   - Bob can now compute: `s_B = s'_B - t`

3. **Mathematical binding**: The same scalar `t` that Alice used to create her signature is exactly what Bob needs to complete his signature.

### Why This Creates Atomicity

**Forward direction** (Alice → Bob):

- If Alice wants her funds, she must publish `s_A`
- Publishing `s_A` reveals `t = s'_A - s_A`
- Bob can compute this and claim his funds

**Reverse direction** (Bob cannot proceed without Alice):

- Bob cannot compute `s_B` without knowing `t`
- Bob cannot learn `t` without Alice publishing `s_A`
- If Alice never publishes, both parties use refund timelocks

**No intermediate state**: There's no way for Alice to claim funds without revealing `t`, and no way for Bob to claim funds before learning `t`.

## Security Analysis

### Atomicity Guarantees

**Success Case**:

1. Alice publishes complete signature → Claims Bob's funds
2. Secret `t` becomes extractable on-chain
3. Bob extracts `t` and publishes complete signature → Claims Alice's funds
4. **Result**: Both swaps complete

**Abort Case**:

1. Alice decides not to proceed
2. Alice never publishes complete signature
3. Refund timelocks expire
4. Both parties reclaim their original funds
5. **Result**: Neither swap completes

**No Partial Execution**: Mathematical impossibility for only one party to succeed.

### Privacy Properties

**External Observer View**:

- Sees two independent multisig transactions
- No hash preimages published on-chain
- No visible connection between transactions
- Transactions appear unrelated (unlinkable)

**Comparison to HTLC**:

- HTLC: Same hash `H(x)` visible on both chains → obvious linkage
- Adapters: Only signatures visible, secret embedded mathematically → no linkage

### Fairness Analysis

**Problem: Initiator Optionality**

The protocol gives Alice (initiator) an unfair advantage:

1. Alice locks funds first
2. Bob locks funds second
3. Alice can wait until near her refund deadline to decide
4. Alice observes market conditions during this period
5. If conditions favor Alice: proceed; if not: wait for refund

**Economic Impact**: This creates an American call option worth 2-3% of swap value for cryptocurrencies.

**Solutions**:

1. **Premium Mechanism** (EIP-2266 approach):
   - Alice pays upfront premium to Bob
   - Premium refundable if Bob fails to participate
   - Premium claimable by Bob if Alice aborts
   - Fair compensation for optionality

2. **Symmetric Commitment**:
   - Both parties lock simultaneously (atomic multisig setup)
   - Eliminate sequential revelation windows
   - Requires protocol modifications

3. **Time-Bounded Execution**:
   - Short decision window after both parties fund
   - Automatic abort if Alice doesn't proceed quickly
   - Reduces optionality value

### Attack Resistance

**Double-Spend Attempts**:

- Prevented by multisig requirements
- Neither party can unilaterally spend locked funds
- Both signatures required for any spend

**Selective Abort**:

- Alice can abort, but reveals nothing
- Both parties recover funds via timelock
- No information leakage during abort

**Griefing Attacks**:

- Alice could lock Bob's funds temporarily
- Mitigated by proper timelock design
- Bob's timelock should expire before Alice's

**Adapter Leakage**:

- If adapter signatures leak before funding, no security impact
- Adapters are verifiable but non-executable without secrets
- Secret `t` never leaves Alice's control until she executes

## Implementation Guidance

### For Our rEdDSA Tutorial

Our Zhu et al. (2024) paper focuses on **single-party** rEdDSA (one signer, one publisher). For two-party atomic swaps, we need:

#### Role Definitions

**Alice (Initiator/Seller)**:

- Generates adapter secret `t` and commitment `T`
- Creates adapter signature for Bob's transaction
- Publishes first to claim funds and reveal secret

**Bob (Responder/Buyer)**:

- Verifies Alice's adapter signature
- Creates adapter signature using Alice's `T`
- Extracts secret and publishes second

#### Key Differences from Single-Party

| Aspect            | Single-Party (Paper)           | Two-Party (Atomic Swap)         |
| ----------------- | ------------------------------ | ------------------------------- |
| Secret generation | Publisher generates `t`        | Alice (initiator) generates `t` |
| Adapter creation  | Signer creates adapter         | Both parties create adapters    |
| Secret usage      | Same `T` used once             | Same `T` used in both adapters  |
| Execution         | Publisher adapts and publishes | Sequential: Alice then Bob      |

#### Protocol Adaptation for rEdDSA

1. **Setup Phase**:

   ```haskell
   -- Alice generates adapter secret
   aliceSecret :: Scalar
   aliceSecret = randomScalar

   aliceCommitment :: Point
   aliceCommitment = aliceSecret `scalarMult` basePoint
   ```

2. **Adapter Creation**:

   ```haskell
   -- Alice creates adapter for Bob's transaction
   aliceAdapter :: AdapterSignature
   aliceAdapter = preSign aliceKey bobTx aliceCommitment

   -- Bob verifies Alice's adapter
   preVerify bobKey bobTx aliceAdapter aliceCommitment

   -- Bob creates adapter for Alice's transaction using same commitment
   bobAdapter :: AdapterSignature
   bobAdapter = preSign bobKey aliceTx aliceCommitment
   ```

3. **Execution**:

   ```haskell
   -- Alice adapts and publishes
   aliceCompleteSignature = adapt aliceAdapter aliceSecret
   publish aliceCompleteSignature bobTx

   -- Bob extracts secret
   extractedSecret = extract aliceAdapter aliceCompleteSignature

   -- Bob adapts and publishes
   bobCompleteSignature = adapt bobAdapter extractedSecret
   publish bobCompleteSignature aliceTx
   ```

### Implementation Checklist

- [ ] Alice generates adapter secret `t` and commitment `T = tG`
- [ ] Both parties exchange public keys and nonces
- [ ] Both parties create 2-of-2 multisig addresses
- [ ] Both parties fund multisig addresses (with refund timelocks)
- [ ] Alice creates adapter signature using `T`
- [ ] Bob verifies Alice's adapter
- [ ] Bob creates adapter signature using same `T`
- [ ] Alice verifies Bob's adapter
- [ ] Alice completes signature and publishes (reveals `t`)
- [ ] Bob extracts `t` from Alice's published signature
- [ ] Bob completes signature and publishes
- [ ] Swap complete: both parties have exchanged assets

### Security Requirements

1. **Proper Timelock Design**:
   - Bob's refund: 24 hours
   - Alice's refund: 48 hours
   - Ensures Bob can always respond before Alice can refund

2. **Adapter Verification**:
   - Both parties MUST verify adapters before funding
   - Invalid adapter = potential fund loss

3. **Secret Generation**:
   - Alice generates `t` using cryptographically secure randomness
   - Never reuse `t` across swaps (linkability + key compromise)

4. **Multisig Security**:
   - Use standard 2-of-2 multisig patterns
   - Verify both parties can sign before funding

## Code Pseudocode

### Data Types

```haskell
-- Scalar field element (private keys, secrets, nonces)
type Scalar = Integer

-- Elliptic curve point (public keys, commitments)
type Point = (Integer, Integer)

-- Adapter signature components
data AdapterSignature = AdapterSignature
  { adapterS :: Scalar       -- s' = r + t + H(...)·x
  , adapterR :: Point        -- R = rG
  , adapterT :: Point        -- T = tG
  }

-- Complete signature components
data Signature = Signature
  { sigS :: Scalar           -- s = r + H(...)·x
  , sigR :: Point            -- R = rG
  }

-- Swap participant state
data Participant = Participant
  { privateKey :: Scalar
  , publicKey :: Point
  , nonce :: Scalar
  , noncePoint :: Point
  }
```

### Core Operations

```haskell
-- Generate adapter secret (Alice only)
generateAdapterSecret :: IO (Scalar, Point)
generateAdapterSecret = do
  t <- randomScalar
  let tPoint = scalarMult t basePoint
  return (t, tPoint)

-- Create adapter signature
preSign :: Participant -> Point -> ByteString -> AdapterSignature
preSign participant adaptorT message =
  let r = nonce participant
      x = privateKey participant
      rPoint = noncePoint participant
      -- Challenge includes adaptor commitment
      challenge = hash (rPoint <> adaptorT <> publicKey participant <> message)
      -- Adapter signature includes secret contribution
      sAdapted = r + t + challenge * x
  in AdapterSignature
       { adapterS = sAdapted
       , adapterR = rPoint
       , adapterT = adaptorT
       }

-- Verify adapter signature
preVerify :: Point -> ByteString -> AdapterSignature -> Bool
preVerify pubKey message adapter =
  let s' = adapterS adapter
      r = adapterR adapter
      t = adapterT adapter
      c = hash (r <> t <> pubKey <> message)
      -- Verification: s'G = R + T + cP
      lhs = scalarMult s' basePoint
      rhs = pointAdd (pointAdd r t) (scalarMult c pubKey)
  in lhs == rhs

-- Adapt signature (add/remove secret)
adapt :: AdapterSignature -> Scalar -> Signature
adapt adapter secret =
  Signature
    { sigS = adapterS adapter - secret  -- Remove t to get valid signature
    , sigR = adapterR adapter
    }

-- Extract secret from signatures
extract :: AdapterSignature -> Signature -> Scalar
extract adapter completeSig =
  -- t = s' - s
  adapterS adapter - sigS completeSig

-- Verify complete signature
verify :: Point -> ByteString -> Signature -> Bool
verify pubKey message sig =
  let s = sigS sig
      r = sigR sig
      c = hash (r <> pubKey <> message)
      -- Standard verification: sG = R + cP
      lhs = scalarMult s basePoint
      rhs = pointAdd r (scalarMult c pubKey)
  in lhs == rhs
```

### Complete Protocol Implementation

```haskell
-- Phase 1: Setup
setupAlice :: IO (Participant, Scalar, Point)
setupAlice = do
  a <- randomScalar
  let aPoint = scalarMult a basePoint
  rA <- randomScalar
  let rAPoint = scalarMult rA basePoint
  (t, tPoint) <- generateAdapterSecret
  return (Participant a aPoint rA rAPoint, t, tPoint)

setupBob :: IO Participant
setupBob = do
  b <- randomScalar
  let bPoint = scalarMult b basePoint
  rB <- randomScalar
  let rBPoint = scalarMult rB basePoint
  return (Participant b bPoint rB rBPoint)

-- Phase 2: Adapter signature exchange
aliceCreatesAdapter :: Participant -> Point -> ByteString -> AdapterSignature
aliceCreatesAdapter alice tPoint bobTransaction =
  preSign alice tPoint bobTransaction

bobVerifiesAliceAdapter :: Point -> ByteString -> AdapterSignature -> Bool
bobVerifiesAliceAdapter alicePubKey bobTransaction aliceAdapter =
  preVerify alicePubKey bobTransaction aliceAdapter

bobCreatesAdapter :: Participant -> Point -> ByteString -> AdapterSignature
bobCreatesAdapter bob tPoint aliceTransaction =
  preSign bob tPoint aliceTransaction

aliceVerifiesBobAdapter :: Point -> ByteString -> AdapterSignature -> Bool
aliceVerifiesBobAdapter bobPubKey aliceTransaction bobAdapter =
  preVerify bobPubKey aliceTransaction bobAdapter

-- Phase 3: Execution
aliceExecute :: AdapterSignature -> Scalar -> Signature
aliceExecute aliceAdapter secret =
  adapt aliceAdapter secret

bobExtract :: AdapterSignature -> Signature -> Scalar
bobExtract aliceAdapter aliceComplete =
  extract aliceAdapter aliceComplete

bobExecute :: AdapterSignature -> Scalar -> Signature
bobExecute bobAdapter extractedSecret =
  adapt bobAdapter extractedSecret

-- Complete protocol
atomicSwap :: IO ()
atomicSwap = do
  -- Setup
  (alice, secret, commitment) <- setupAlice
  bob <- setupBob

  putStrLn "Phase 1: Setup complete"
  putStrLn $ "  Alice public key: " ++ show (publicKey alice)
  putStrLn $ "  Bob public key: " ++ show (publicKey bob)
  putStrLn $ "  Adapter commitment T: " ++ show commitment

  -- Fund multisig addresses (not shown: actual blockchain interaction)
  let bobTx = "Bob pays Alice X BTC"
  let aliceTx = "Alice pays Bob Y altcoins"

  -- Alice creates and Bob verifies adapter
  let aliceAdapter = aliceCreatesAdapter alice commitment bobTx
  guard (bobVerifiesAliceAdapter (publicKey alice) bobTx aliceAdapter)
  putStrLn "Phase 2: Alice's adapter verified"

  -- Bob creates and Alice verifies adapter
  let bobAdapter = bobCreatesAdapter bob commitment aliceTx
  guard (aliceVerifiesBobAdapter (publicKey bob) aliceTx bobAdapter)
  putStrLn "Phase 2: Bob's adapter verified"

  -- Alice executes
  let aliceComplete = aliceExecute aliceAdapter secret
  guard (verify (publicKey alice) bobTx aliceComplete)
  putStrLn "Phase 3: Alice published signature and claimed funds"

  -- Bob extracts and executes
  let extractedSecret = bobExtract aliceAdapter aliceComplete
  guard (extractedSecret == secret)
  let bobComplete = bobExecute bobAdapter extractedSecret
  guard (verify (publicKey bob) aliceTx bobComplete)
  putStrLn "Phase 3: Bob extracted secret and claimed funds"

  putStrLn "Atomic swap complete!"
```

### Testing Scenarios

```haskell
-- Test 1: Successful swap
testSuccessfulSwap :: IO Bool
testSuccessfulSwap = do
  (alice, secret, commitment) <- setupAlice
  bob <- setupBob

  let aliceAdapter = aliceCreatesAdapter alice commitment "msg1"
  let bobAdapter = bobCreatesAdapter bob commitment "msg2"

  -- Verify adapters
  guard (bobVerifiesAliceAdapter (publicKey alice) "msg1" aliceAdapter)
  guard (aliceVerifiesBobAdapter (publicKey bob) "msg2" bobAdapter)

  -- Alice executes
  let aliceComplete = aliceExecute aliceAdapter secret
  guard (verify (publicKey alice) "msg1" aliceComplete)

  -- Bob extracts and executes
  let extracted = bobExtract aliceAdapter aliceComplete
  guard (extracted == secret)
  let bobComplete = bobExecute bobAdapter extracted
  guard (verify (publicKey bob) "msg2" bobComplete)

  return True

-- Test 2: Adapter verification
testAdapterVerification :: IO Bool
testAdapterVerification = do
  (alice, secret, commitment) <- setupAlice
  bob <- setupBob

  let adapter = aliceCreatesAdapter alice commitment "test message"

  -- Should verify successfully
  let valid = preVerify (publicKey alice) "test message" adapter

  -- Should fail with wrong message
  let invalid = preVerify (publicKey alice) "wrong message" adapter

  return (valid && not invalid)

-- Test 3: Secret extraction
testSecretExtraction :: IO Bool
testSecretExtraction = do
  (alice, secret, commitment) <- setupAlice

  let adapter = aliceCreatesAdapter alice commitment "message"
  let complete = aliceExecute adapter secret
  let extracted = bobExtract adapter complete

  return (extracted == secret)
```

## Comparison: Different Approaches

### Basic Adapter Swap (Blockstream scriptless-scripts)

**Characteristics**:

- Both chains require multisig support
- Sequential execution (Alice first, Bob second)
- Timelock-based refunds
- Four transactions total (2 funding, 2 claims)

**Advantages**:

- Simple conceptually
- Well-understood security model
- Minimal blockchain requirements

**Disadvantages**:

- Four on-chain transactions
- Sequential dependency creates timing vulnerability
- Initiator optionality problem

### Succinct Atomic Swap (Ruben Somsen)

**Characteristics**:

- Only two on-chain transactions (best case)
- Pre-signing all transactions before funding
- Off-chain adapter exchange
- One chain needs minimal script support

**Advantages**:

- Most efficient on-chain footprint
- Can work with one scriptless chain
- Reduced transaction costs

**Disadvantages**:

- More complex pre-signing ceremony
- Requires careful state management
- Still has optionality issues

### Premium-Based Fair Swap (EIP-2266)

**Characteristics**:

- Additional premium payment transaction
- Premium claimable by responder if initiator aborts
- Premium refundable if responder fails to participate
- Addresses American call option problem

**Advantages**:

- Economically fair to both parties
- Compensates for optionality
- Standard contract interface (for Ethereum)

**Disadvantages**:

- Additional transaction overhead
- Requires premium price discovery
- More complex protocol

### Bitcoin-Monero (Farcaster Protocol)

**Characteristics**:

- Asymmetric requirements (Bitcoin has scripts, Monero doesn't)
- Cross-group cryptography (secp256k1 ↔ curve25519)
- ECDSA adapters on Bitcoin, Schnorr on Monero
- Zero-knowledge proofs for cross-curve linkage

**Advantages**:

- Works with very different blockchain architectures
- Maintains Monero's privacy properties
- Real-world implementation exists

**Disadvantages**:

- Complex cryptographic constructions
- Zero-knowledge proof overhead
- Requires both ECDSA and Schnorr adapter implementations

### Multi-Party Universal Adaptor (Recent Research)

**Characteristics**:

- Single universal adapter secret for N parties
- Scalable to multiple blockchains
- Atomic release across all parties

**Advantages**:

- Extends to N-party swaps
- Single secret for all transactions
- Generalizes two-party construction

**Disadvantages**:

- More complex setup
- Research-stage (not widely deployed)
- Coordinating N parties challenging

## Recommendations for Tutorial Implementation

### For Educational Purposes

**Start with**: Basic two-party Schnorr adapter swap

- Clearest mathematical structure
- Aligns with rEdDSA paper's foundation
- Easy to explain and verify

**Protocol Flow**:

1. Alice generates secret and commitment
2. Both parties exchange adapters
3. Alice executes first (reveals secret)
4. Bob extracts and executes

### For Production Use

**Consider**: Succinct Atomic Swap variant

- Minimal on-chain footprint
- Production-tested design
- Good privacy properties

**Add**: Premium mechanism for fairness

- Compute fair premium (2-3% for crypto)
- Implement claim/refund logic
- Document economic considerations

### For Maximum Compatibility

**Use**: ECDSA adapters for Bitcoin compatibility

- Bitcoin uses ECDSA (secp256k1)
- Cardano uses Ed25519
- Need cross-curve protocol (like Farcaster)

**Alternative**: Wait for Schnorr adoption

- Bitcoin Taproot enables Schnorr
- Simpler mathematical structure
- Better privacy and efficiency

## Future Research Directions

### Open Problems

1. **Fair Exchange Without Premium**:
   - Can protocol design eliminate optionality?
   - Simultaneous revelation mechanisms?
   - Lightning Network approach: repeated interactions build reputation

2. **Quantum-Resistant Adapters**:
   - Post-quantum signature schemes
   - Lattice-based constructions
   - Performance implications

3. **Multi-Asset Swaps**:
   - Three-way swaps (A→B→C→A)
   - Conditional swaps (if-then patterns)
   - Order book integration

4. **Cross-Curve Efficiency**:
   - Lighter zero-knowledge proofs
   - Universal elliptic curve adapters
   - Performance benchmarks

### Implementation Challenges

1. **Blockchain Integration**:
   - Transaction monitoring
   - Multisig address generation
   - Timelock implementation
   - Fee management

2. **Network Communication**:
   - Peer discovery
   - Adapter exchange protocol
   - Fallback mechanisms
   - Tor/privacy network support

3. **User Experience**:
   - Automated market maker integration
   - Fiat on/off ramps
   - Mobile wallet support
   - Recovery procedures

## References

### Academic Papers

1. **Gugger, J. (2020)**. "Bitcoin–Monero Cross-chain Atomic Swap"
   - ePrint: https://eprint.iacr.org/2020/1126
   - Foundation for Bitcoin-Monero swaps

2. **Fournier, L. (2019)**. "One-Time Verifiably Encrypted Signatures A.K.A. Adaptor Signatures"
   - Formal definition of adaptor signatures
   - Security proofs

3. **Han, R. et al. (2019)**. "On the optionality and fairness of Atomic Swaps"
   - ePrint: https://eprint.iacr.org/2019/896
   - American call option analysis
   - Premium mechanism proposal

4. **Deshpande, A. et al. (2024)**. "A Multi-Party, Multi-Blockchain Atomic Swap Protocol with Universal Adaptor Secret"
   - arXiv: https://arxiv.org/abs/2406.16822
   - N-party generalization

5. **Zhu, Y. et al. (2024)**. "rEdDSA: A Variant of EdDSA with Controllable Randomness and Applications"
   - Foundation for our implementation
   - Single-party adapter construction

### Technical Resources

6. **Poelstra, A. (2017)**. "Scriptless Scripts"
   - Original scriptless scripts concept
   - Adaptor signature introduction

7. **Bitcoin Optech**. "Adaptor Signatures"
   - URL: https://bitcoinops.org/en/topics/adaptor-signatures/
   - Comprehensive overview and examples

8. **Blockstream Research**. "Scriptless Scripts Repository"
   - URL: https://github.com/BlockstreamResearch/scriptless-scripts
   - Detailed protocol specifications

9. **Somsen, R. (2020)**. "Succinct Atomic Swaps"
   - Gist: https://gist.github.com/RubenSomsen/8853a66a64825716f51b409be528355f
   - Efficient two-transaction protocol

10. **Farcaster Project**. "Bitcoin-Monero Atomic Swap RFCs"
    - GitHub: https://github.com/farcaster-project/RFCs
    - Production implementation specs

### Standards

11. **EIP-2266**: "Atomic Swap-based American Call Option Contract Standard"
    - URL: https://eips.ethereum.org/EIPS/eip-2266
    - Fair premium mechanism

### Educational Resources

12. **conduition.io**. "The Riddles of Adaptor Signatures"
    - URL: https://conduition.io/scriptless/adaptorsigs/
    - Clear technical explanations

13. **Tari Labs University**. "Introduction to Scriptless Scripts"
    - URL: https://tlu.tarilabs.com/cryptography/introduction-to-scriptless-scripts
    - Educational material

## Appendix: Mathematical Foundations

### Schnorr Signatures Recap

Standard Schnorr signature on message `m`:

- Private key: `x`, Public key: `P = xG`
- Nonce: `r`, Nonce point: `R = rG`
- Challenge: `c = H(R || P || m)`
- Signature: `s = r + cx`
- Verification: `sG = R + cP`

### Adapter Signature Construction

Adapter signature with secret `t` and commitment `T = tG`:

- Adapter challenge: `c = H((R + T) || P || m)`
- Adapter signature: `s' = r + t + cx`
- Pre-verification: `s'G = R + T + cP`
- Complete signature: `s = s' - t = r + cx`
- Extraction: `t = s' - s`

### Why Extraction Works

Given adapter `(s', R, T)` and complete signature `(s, R)`:

```
s' = r + t + cx   (adapter equation)
s  = r + cx       (signature equation)
```

Subtracting:

```
s' - s = (r + t + cx) - (r + cx)
       = t
```

Therefore, anyone can compute `t = s' - s`.

### Security Reduction

**Theorem**: If the discrete logarithm problem is hard, then:

1. Adapter signatures are unforgeable
2. Secret extraction is always possible
3. Adapters and complete signatures are indistinguishable to external observers

**Proof sketch**:

- Unforgeability reduces to standard Schnorr unforgeability
- Extraction is deterministic (arithmetic over scalars)
- Indistinguishability follows from uniformity of `r + t` mod `q`

---

**Document Status**: Research complete as of 2025-11-14. Ready for tutorial implementation.

**Next Steps**:

1. Implement basic two-party protocol in Haskell
2. Add test vectors for adapter signature operations
3. Create interactive tutorial with step-by-step execution
4. Consider premium mechanism for fairness
5. Integrate with existing rEdDSA implementation
