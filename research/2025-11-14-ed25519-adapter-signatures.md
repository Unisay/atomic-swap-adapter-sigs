---
Generated: 2025-11-14 15:30
Type: research
---

# Ed25519 Adapter Signatures for Atomic Swaps: Research Report

## Executive Summary

### Are Ed25519 Adapter Signatures Feasible?

**Yes, with caveats.** Ed25519 adapter signatures are theoretically feasible and have been demonstrated in real-world atomic swap implementations (Bitcoin-Monero swaps). However, they present significant complications compared to standard Schnorr signatures on prime-order curves like secp256k1.

### Key Findings

1. **EdDSA is a Schnorr Variant**: Ed25519 is a variant of Schnorr signatures, which means the core linearity property needed for adapter signatures exists.

2. **Cofactor Complications**: Ed25519's cofactor of 8 creates security challenges for complex protocols like adapter signatures that don't exist with cofactor-1 curves.

3. **Recent Academic Work**: A 2024 paper by Zhu et al. presents a complete construction of "Adaptor signature based on randomized EdDSA in blockchain" with formal security proofs and implementation details (see detailed analysis below).

4. **Production Implementations**: The Farcaster project successfully implements Bitcoin-Monero atomic swaps, bridging secp256k1 and ed25519 curves using zero-knowledge proofs.

### Recommended Approach for Educational Implementation

**For maximum pedagogical clarity**: Implement Schnorr adapter signatures on secp256k1 first, then discuss Ed25519 complications as advanced topics.

**Rationale**:

- Clearer security properties (no cofactor issues)
- More established academic literature
- Simpler implementation without clamping complications
- Better alignment with Bitcoin/Lightning Network examples

**If Ed25519 is required**: Consider using Ristretto255 abstraction layer to eliminate cofactor-related vulnerabilities while maintaining Ed25519 compatibility.

---

## Background: Adapter Signatures

### What Are Adapter Signatures?

Adapter signatures (also called **adaptor signatures** or **verifiably encrypted signatures**) are a cryptographic primitive that enables:

1. **Conditional Signatures**: A signature that's "locked" to a secret value
2. **Secret Revelation**: Publishing the signature reveals the secret
3. **Scriptless Scripts**: Smart contract functionality without on-chain scripts

### Core Properties

An adapter signature scheme must satisfy three security properties:

1. **Unforgeability**: Cannot create valid signatures without the private key
2. **Pre-signature Adaptability**: Pre-signatures can be converted to valid signatures with the adaptor secret
3. **Witness Extractability**: The adaptor secret can be extracted from signature pairs

### Use Cases

- **Atomic Swaps**: Cross-chain cryptocurrency exchanges without trusted intermediaries
- **Payment Channels**: Off-chain payment networks (Lightning Network)
- **Discreet Log Contracts**: Privacy-preserving smart contracts
- **Multi-party Protocols**: Threshold signatures and MPC applications

### How They Work (Conceptual)

```
Setup Phase:
- Adaptor point: T = t·G (t is secret)
- Participants agree on T publicly

Signature Creation:
- Generate nonce: R̂ = k·G
- Adapt nonce: R = R̂ + T
- Create challenge: c = H(R || A || m)
- Pre-signature: ŝ = k + c·a

Signature Adaptation:
- Adapted signature: s = ŝ + t
- Now (R, s) is a valid signature

Secret Extraction:
- Observer sees ŝ and s
- Can compute: t = s - ŝ
```

---

## Ed25519 vs Schnorr: Critical Differences

### Similarities (Why Adapter Signatures Might Work)

1. **Both are Schnorr-style**: EdDSA is explicitly "a variant of Schnorr signature based on twisted Edwards curves"
2. **Linearity Property**: Both have the linear signature equation: `s = k + c·a`
3. **Algebraic Structure**: Both support the additive homomorphism needed for adaptation

### Critical Differences (Why Ed25519 is More Complex)

#### 1. Cofactor Issues

| Property              | Schnorr (secp256k1) | Ed25519                     |
| --------------------- | ------------------- | --------------------------- |
| Cofactor              | h = 1               | h = 8                       |
| Group Order           | Prime               | Composite (8 × large prime) |
| Security Implications | Simpler proofs      | Requires careful handling   |

**Impact on Adapter Signatures**:

- Ed25519's cofactor-8 creates small subgroup elements
- Protocols must carefully handle points that aren't in the prime-order subgroup
- Malleability issues: Multiple representations of the same point possible
- Security proofs become significantly more complex

**Historical Vulnerability**: The CryptoNote scheme used by Monero had an octuple-spend vulnerability due to Ed25519 malleability, where adversaries could add low-order points to existing transactions.

#### 2. Scalar Clamping

**Ed25519 Clamping Convention**:

```
Private key s is modified:
- Clear bits 0, 1, 2 (ensures multiple of 8)
- Clear bit 255
- Set bit 254

Result: s ∈ {8, 16, 24, ..., 2^254}
```

**Why It Exists**: Makes basic protocols secure despite cofactor issues

**Problem for Adapter Signatures**:

- Restricts the space of valid secrets
- Complicates secret extraction
- May need to be dropped for some protocols
- Affects compatibility with standard Ed25519 implementations

#### 3. Hash Function and Signature Format

| Aspect           | Schnorr                    | Ed25519                            |
| ---------------- | -------------------------- | ---------------------------------- |
| Hash Function    | SHA-256 or domain-specific | SHA-512 (mandated)                 |
| Signature Format | (R, s)                     | (R, S) with different encoding     |
| Key Derivation   | Various schemes            | Deterministic from seed            |
| Nonce Generation | RFC 6979 or random         | Deterministic: r = H(h[32:64] ‖ m) |

**Impact**: Ed25519's deterministic nonce generation must be carefully handled when creating pre-signatures to avoid revealing the private key.

#### 4. Curve Arithmetic

**Edwards Curve Form**: `ax² + y² = 1 + dx²y²`

**Benefits**:

- Complete addition formulas (no special cases)
- Faster point operations
- Unified point representation

**Challenges for Adapter Signatures**:

- Different point encoding than Weierstrass curves
- Compression/decompression considerations
- Compatibility with other curves in cross-chain scenarios

---

## Existing Constructions and Research

### Academic Literature

#### 1. "Adaptor signature based on randomized EdDSA in blockchain" (2024) ⭐ **PRIMARY REFERENCE**

**Authors**: Yixing Zhu, Huilin Li, Mengze Li, Yong Yu
**Affiliation**: School of Computer Science, Shaanxi Normal University, Xi'an, China

**Source**: Digital Communications and Networks, Volume 11 (2025) 689-699

- DOI: 10.1016/j.dcan.2024.06.004
- Received: January 18, 2024; Accepted: June 6, 2024
- Published: June 12, 2024
- Open Access (CC BY-NC-ND)

**Abstract Summary**:
Proposes a new adapter signature construction using **randomized EdDSA** with Schnorr-like structure, achieving higher signing efficiency and shorter signature length than existing ECDSA-based schemes. Proves required security properties in the random oracle model.

---

### Complete Technical Details from Paper

#### Motivation

**Problem**: Deterministic EdDSA cannot be directly used for adapter signatures due to risk of leaking the adapter secret.

**Solution**: Use **randomized EdDSA** (rEdDSA) which embeds a random value in the signature, enabling adapter signature construction without sacrificing efficiency.

**Benefits over ECDSA-based adapters**:

1. Higher signing efficiency (twisted Edwards curves optimization)
2. Shorter signature length (64 bytes for Ed25519 vs 192 bytes for ECDSA pre-signature)
3. Compact public keys (32 bytes for Ed25519)

#### Randomized EdDSA (rEdDSA) Signature Scheme

**Public Parameters**: `EdPP = (Ep, 𝔹, q, B, b, H1, H2)`

- `Ep`: twisted elliptic curve
- `𝔹`: additive cycle group with generator `B` and order `q`
- `b`: bit length of secret scalars
- `H1: {0,1}^b → {0,1}^2b` (key derivation hash)
- `H2: {0,1}* → ℤ_n*` (signature hash)

**KeyGen(EdPP)**:

```
1. Select random b-bit string t as private key
2. Compute H1(t) = (h0, h1, ..., h_{2b-1})
3. Calculate sk0 = 2^n + Σ_{c≤i<n} 2^i · h_i
   (where c ∈ {2,3} and c ≤ n < b)
4. Calculate public key: pk = sk0 · B
5. Define sk1 = (h_b, h_{b+1}, ..., h_{2b-1})
6. Output (sk0, sk1, pk)
```

**Sign(sk0, sk1, m)**:

```
1. Select random k ∈ ℤ_n*
2. Calculate random nonce: r = H2(sk1 || m || k) mod q
3. Compute R = r · B
4. Compute challenge: h = H2(R || pk || m)
5. Compute signature scalar: sig = r + h · sk0 mod q
6. Output σ = (sig, R)
```

**Verify(pk, m, σ)**:

```
Parse σ = (sig, R)
Check: sig · B = R + H2(R || pk || m) · pk
Output 1 if valid, 0 otherwise
```

#### rEdDSA-based Adapter Signature Construction

**Hard Relation**: `L_rEd,R := {(Y, y) : Y = y · B}`

**Algorithms**: `Φ_{R,rEdDSA} = {Setup, KeyGen, GenR, pSign, pVerify, Adapt, Ext}`

##### 1. Setup(1^n)

```
Output EdPP = (Ep, 𝔹, q, B, b, H1, H2)
```

##### 2. KeyGen(EdPP)

```
Same as rEdDSA KeyGen
Output (sk0, sk1, pk)
```

##### 3. GenR(EdPP) - Generate Hard Relation

```
1. Publisher selects witness y ∈ ℤ_n*
2. Calculate statement Y = y · B
3. Generate NIZK proof: π = Prove_zk(Y, y)
4. Output ((Y, y), π)
```

##### 4. pSign(sk0, sk1, m, Y, π) - Pre-Signature Generation

```
1. Select random k ∈ ℤ_n*
2. Calculate r = H2(sk1 || m || k) mod q
3. Compute R_pre = r · B
4. Compute R_sign = R_pre + Y  ← KEY STEP: Add adapter point!
5. Calculate challenge: h = H2(R_sign || pk || m)
6. Compute pre-signature scalar: sig_tilde = r + h · sk0 mod q
   (Note: does NOT include y!)
7. Output σ_tilde = (sig_tilde, R_sign, π)
```

**Key Insight**: The pre-signature is computed WITHOUT the adapter secret `y`, but the nonce point is adapted by adding `Y = y·B`. This creates a "lock" that requires `y` to complete.

##### 5. pVerify(pk, m, Y, σ_tilde) - Pre-Signature Verification

```
1. Parse σ_tilde = (sig_tilde, R_sign, π)
2. Calculate R' = R_sign - Y  ← Remove adapter point
3. Calculate h = H2(R_sign || pk || m)
4. Verify NIZK proof: b_zk = Verify_zk(Y, π)
5. Check equation: sig_tilde · B = R' + h · pk
6. Output 1 if (equation holds ∧ b_zk = 1), else 0
```

**Why this works**:

```
sig_tilde · B = (r + h · sk0) · B
             = r · B + h · sk0 · B
             = r · B + h · pk
             = (R_sign - Y) + h · pk  (since R_sign = r·B + Y)
             = R' + h · pk  ✓
```

##### 6. Adapt(σ_tilde, y) - Complete the Signature

```
1. Parse σ_tilde = (sig_tilde, R_sign, π)
2. Compute complete signature scalar: sig = sig_tilde + y
3. Output σ = (R_sign, sig)
```

**Result**: Now `(R_sign, sig)` is a valid rEdDSA signature!

**Verification**:

```
sig · B = (sig_tilde + y) · B
        = sig_tilde · B + y · B
        = R' + h · pk + Y  (from pre-signature verification)
        = (R_sign - Y) + h · pk + Y
        = R_sign + h · pk  ✓
```

##### 7. Ext(σ_tilde, σ, Y) - Extract Adapter Secret

```
1. Parse σ_tilde = (sig_tilde, R_sign, π)
2. Parse σ = (R_sign, sig)
3. Extract secret: y = sig - sig_tilde
4. Check (Y, y) ∈ L_rEd,R
5. If valid, output y; else output Invalid
```

**THE MAGIC**: Anyone who sees both the pre-signature `sig_tilde` and the complete signature `sig` can compute the adapter secret: `y = sig - sig_tilde`

---

#### Security Proofs

**Theorem 1**: The rEdDSA-based adapter signature scheme `Φ_{R,rEdDSA}` is a secure adapter signature scheme in the random oracle model, assuming:

1. rEdDSA is EUF-CMA secure
2. R is a hard relation

**Proven Properties**:

1. **Pre-signature Correctness** (Lemma 2):
   - A correctly generated pre-signature can be verified
   - Adapting it with witness y yields a valid signature
   - Extracting from pre-signature and signature recovers y

2. **Pre-signature Adaptability** (Lemma 1):
   - If pVerify(pk, m, Y, σ_tilde) = 1
   - Then Verify(pk, m, Adapt(σ_tilde, y)) = 1

3. **aEUF-rEdDSA-CMA Security** (Lemma 3):
   - Adversary with access to signing and pre-signing oracles
   - Cannot forge valid signature for new message
   - Reduction to EUF-CMA security of underlying rEdDSA

4. **Witness Extractability** (Lemma 4):
   - Adversary who forges statement Y\* and signature
   - Cannot produce valid signature where extracted witness doesn't satisfy relation
   - Reduction to EUF-CMA security of rEdDSA

**Proof Technique**: Game-based reductions using random oracle model

- Game hops from initial experiment to underlying rEdDSA security
- Simulator replaces pre-signatures with complete rEdDSA signatures plus randomness
- Indistinguishability relies on random oracle and hard relation assumptions

---

#### Implementation and Performance

**Experimental Setup**:

- Curve: Ed25519
- Hardware: AMD Ryzen 7 3700X @ 3.6 GHz, 16 GB RAM
- Language: Python 3
- IDE: PyCharm 2023.2.1

**Comparison with ECDSA-based Adapter Signatures**:

| Algorithm | ECDSA-based            | rEdDSA-based    | Improvement                       |
| --------- | ---------------------- | --------------- | --------------------------------- |
| GenR      | t_p + t_m              | t_p + t_m       | Same (curve-dependent)            |
| pSign     | 2t_m + t_h + t_i       | t_m + 2t_h      | Faster (no modular inverse)       |
| pVerify   | t_v + 2t_m + t_h + t_i | t_v + t_m + t_h | Faster                            |
| Adapt     | t_i                    | t_add           | Much faster (addition vs inverse) |
| Ext       | t_i                    | t_add           | Much faster                       |

Where:

- t_m: point multiplication
- t_p, t_v: NIZK prove/verify
- t_h: SHA-512 hash
- t_i: modular inverse
- t_add: modular addition

**Communication Overhead**:

| Component     | ECDSA-based | rEdDSA-based | Savings |
| ------------- | ----------- | ------------ | ------- | --- | ----------- | --- | ----- | --- | --- | ----------- | --- |
| Proof size    | 3           | ℤ_n\*        | +       | G1  | = 160 bytes | 2   | ℤ_n\* | + 2 | G2  | = 128 bytes | 20% |
| Pre-signature | 4           | ℤ_n\*        | +       | G1  | = 192 bytes | 2   | ℤ_n\* | + 2 | G2  | = 128 bytes | 33% |

**Key Advantages**:

1. ✅ Faster signing (no modular inverse in pSign)
2. ✅ Faster verification
3. ✅ Much faster Adapt/Ext (addition vs. inverse)
4. ✅ Smaller signatures (33% reduction)
5. ✅ Smaller proofs (20% reduction)

---

#### Application to Atomic Swaps

**Use Case**: Off-chain payment channels and atomic swaps

**Protocol Integration**:

1. Signer generates pre-signature with adapter point
2. Intermediate nodes can verify pre-signature validity
3. Publisher completes signature by adding adapter secret
4. All observers can extract secret from published signature

**Advantages over HTLC**:

- Resists wormhole attacks
- Enhanced security and privacy
- Better interoperability guarantees
- Scriptless scripts (no on-chain smart contracts needed)

**Blockchain Compatibility**:

- ✅ **Cardano**: Uses Ed25519 natively
- ✅ **Monero**: Uses Ed25519/Curve25519
- ✅ **Stellar**, **Decred**: Ed25519-based
- Total: ~25% of cryptocurrencies use EdDSA

---

### Assessment for Tutorial Implementation

**Verdict**: ✅ **RECOMMENDED - rEdDSA adapter signatures are well-established**

**Reasons to use this construction**:

1. **Complete specification**: Paper provides full algorithmic details
2. **Formal security proofs**: All required properties proven in ROM
3. **Implementation guidance**: Clear pseudocode and parameters
4. **Better performance**: Faster than ECDSA-based alternatives
5. **Cardano/Monero compatibility**: Direct Ed25519 usage
6. **Recent publication**: State-of-the-art (2024)

**Implementation Checklist**:

- ✅ Randomized EdDSA as base signature scheme
- ✅ NIZK proofs for hard relation (discrete log proof)
- ✅ Ed25519 curve (twisted Edwards)
- ✅ SHA-512 for hashing
- ✅ Modular arithmetic in ℤ_q

**Haskell Libraries Needed**:

1. `cryptonite` or `ed25519` - Ed25519 operations
2. Library for NIZK proofs (may need custom implementation)
3. `memory` - ByteString operations
4. `crypton` - SHA-512 hashing

**Pedagogical Benefits**:

- Clean mathematical structure
- Clear "lock and unlock" metaphor (add Y, add y)
- Simple extraction formula (y = sig - sig_tilde)
- Explicit security reductions
- Real-world applicability

**Next Steps**:

1. Implement rEdDSA signature scheme first
2. Add NIZK proof system for DLog relation
3. Build adapter signature layer on top
4. Create Alice/Bob atomic swap protocol using this construction

#### 2. Generic Constructions from Identification Schemes (2021)

**Paper**: "Two-Party Adaptor Signatures From Identification Schemes"

**Source**: IACR ePrint 2021/150

**Key Contribution**: Shows that signature schemes constructed from identification schemes with homomorphic properties can be generically transformed into adapter signature schemes.

**Applicable Schemes**:

- Schnorr signatures ✓
- Katz-Wang scheme ✓
- Guillou-Quisquater scheme ✓
- BLS signatures ✗ (unique signatures incompatible)

**Relevance to Ed25519**: Since EdDSA is Schnorr-based, this generic construction theoretically applies. However, cofactor issues require special handling not addressed in the base construction.

#### 3. "Foundations of Adaptor Signatures" (2024)

**Source**: IACR ePrint 2024/1809

**Key Innovation**: Introduces "dichotomic signatures" abstraction for building adapter signatures

**Covered Schemes**:

- Camenisch-Lysyanskaya (CL)
- Boneh-Boyen-Shacham (BBS+)
- Waters signatures
- Schnorr signatures (proven secure)

**Ed25519 Status**: Not explicitly addressed in available abstracts

#### 4. Threshold EdDSA/Schnorr Research

**NIST Report**: "Notes on Threshold EdDSA/Schnorr Signatures" (NISTIR 8214B)

**Relevance**:

- Explores threshold variants of EdDSA
- Addresses cofactor handling in multi-party protocols
- Security considerations applicable to adapter signatures

**Key Insight**: "EdDSA is historically known as a variant of Schnorr signatures, well-studied and suitable for efficient thresholdization"

### Production Implementations

#### 1. Farcaster: Bitcoin-Monero Atomic Swaps

**Project**: https://github.com/farcaster-project

**Developers**: h4sh3d and team

**Funding**: Monero Community Crowdfunding System (CCS)

**Technical Approach**:

- **Challenge**: Bridge secp256k1 (Bitcoin) and ed25519 (Monero) curves
- **Solution**: Zero-knowledge proofs of equal discrete logarithm across groups
- **Signature Scheme**: ECDSA one-time Verifiable Encryption Scheme (VES) - adapter signatures
- **Additional Protocol**: No timelocks on Monero side (unlike traditional atomic swaps)

**Protocol Paper**: "Bitcoin–Monero Cross-chain Atomic Swap" (IACR ePrint 2020/1126)

**Key Requirements**:

1. Two proofs of knowledge of equal discrete logarithm across ed25519 and secp256k1
2. ECDSA one-time VES (adapter signatures)
3. Zero-knowledge proofs for cross-curve secret relationships

**Implementation Language**: Rust

**Cryptography Libraries**:

- `dalek-cryptography` for ed25519 operations
- `secp256k1` for Bitcoin signatures

**Status**: Production-ready, used in real atomic swaps

**Significance**: Proves Ed25519 adapter signatures are practical, though with additional complexity (ZK proofs needed for cross-curve operations).

#### 2. AthanorLabs: ETH-XMR Atomic Swaps

**Project**: https://github.com/AthanorLabs/atomic-swap

**Status**: Beta implementation

**Relevant Issue**: #35 discusses adapter signature-based swaps for reduced transaction fees

**Technical Notes**:

- Uses similar approach to Farcaster
- Demonstrates adapter signatures between ECDSA (Ethereum) and Ed25519 (Monero)

#### 3. Cardano Platform

**Signature Scheme**: Edwards25519 (Ed25519) for key pair generation

**Smart Contract Platform**: Plutus (Haskell-based)

**Cryptographic Capabilities**:

- Native Ed25519 support
- SECP curve support added (Valentine upgrade: ECDSA and Schnorr)
- BLS12-381 pairing in PlutusV3 for zero-knowledge proofs

**Atomic Swap Status**:

- Forum discussions about atomic swap use cases
- No publicly documented Ed25519 adapter signature implementations found
- Platform capabilities support implementation

**Relevance**: Shows Ed25519 is chosen by major blockchain platforms, but doesn't provide implementation examples.

---

## Ristretto255: A Solution to Cofactor Problems

### What is Ristretto255?

**Definition**: "A technique for constructing prime order elliptic curve groups with non-malleable encodings"

**Purpose**: Provides a thin abstraction layer over Curve25519 that eliminates cofactor-related vulnerabilities

### How It Works

```
Curve25519 (cofactor 8)
        ↓
[Ristretto255 Abstraction Layer]
        ↓
Prime-order group interface
```

**Key Properties**:

- Built on existing Curve25519 implementations
- Minimal code changes required
- No additional cryptographic assumptions
- Non-malleable point encodings

### Benefits for Ed25519 Protocols

1. **Eliminates Cofactor Attacks**: Presents prime-order group to protocol layer
2. **Prevents Malleability**: Each group element has unique encoding
3. **Simplifies Security Proofs**: Can assume prime-order group properties
4. **Maintains Performance**: Thin layer, minimal overhead

### Applicability to Adapter Signatures

**Theoretical**: Ristretto255 makes Ed25519-based systems "safely extended with zero-knowledge protocols"

**Practical Consideration**: Using Ristretto255 would likely simplify adapter signature implementation by eliminating cofactor handling complexity

**Trade-off**: Adds dependency and may not be compatible with standard Ed25519 signature verification

### Resources

- Website: https://ristretto.group/
- IETF Draft: draft-irtf-cfrg-ristretto255-decaf448
- Use Case: "Systems using Ed25519 signatures can be safely extended with zero-knowledge protocols"

---

## Proposed Approach for Educational Implementation

### Option 1: Schnorr on secp256k1 (RECOMMENDED)

**Rationale**: Maximum pedagogical clarity

#### Advantages

1. **Simpler Security Model**: No cofactor complications
2. **Established Literature**: Extensive documentation and examples
3. **Industry Standard**: Used in Bitcoin Taproot, Lightning Network
4. **Clean Mathematics**: Prime-order group with straightforward proofs

#### Implementation Strategy

**Phase 1: Basic Schnorr Signatures**

```haskell
-- Core types
type PrivateKey = Scalar
type PublicKey = Point
type Signature = (Point, Scalar)  -- (R, s)

-- Key generation
generateKey :: IO (PrivateKey, PublicKey)
generateKey = do
  a <- randomScalar
  let publicKey = a .* basePoint
  return (a, publicKey)

-- Signature creation
sign :: PrivateKey -> Message -> IO Signature
sign privKey msg = do
  k <- randomScalar
  let r = k .* basePoint
      c = hash (r <> pubKey <> msg)
      s = k + c * privKey
  return (r, s)

-- Verification
verify :: PublicKey -> Message -> Signature -> Bool
verify pubKey msg (r, s) =
  let c = hash (r <> pubKey <> msg)
      lhs = s .* basePoint
      rhs = r + c .* pubKey
  in lhs == rhs
```

**Phase 2: Adapter Signatures**

```haskell
-- Additional types
type AdaptorSecret = Scalar
type AdaptorPoint = Point
type PreSignature = Scalar  -- ŝ

-- Pre-signature creation
createPreSignature :: PrivateKey -> Message -> AdaptorPoint -> IO (Point, PreSignature)
createPreSignature privKey msg adaptorPoint = do
  k <- randomScalar
  let rHat = k .* basePoint
      r = rHat + adaptorPoint  -- Adapt the nonce
      c = hash (r <> pubKey <> msg)
      sHat = k + c * privKey  -- Pre-signature
  return (rHat, sHat)

-- Adapt pre-signature to full signature
adaptSignature :: PreSignature -> AdaptorSecret -> Signature
adaptSignature sHat t =
  let s = sHat + t
  in (r, s)  -- Where r was computed earlier

-- Extract secret from signature pair
extractSecret :: PreSignature -> Scalar -> AdaptorSecret
extractSecret sHat s = s - sHat
```

**Phase 3: Atomic Swap Protocol**

```haskell
-- Two-party atomic swap
data SwapParty = Alice | Bob

-- Alice's side (initiator)
aliceInitiate :: IO (AdaptorPoint, PreSignature)
aliceInitiate = do
  t <- randomScalar  -- Adaptor secret (Alice's lock)
  let adaptorPoint = t .* basePoint
  -- Create pre-signature for Alice's transaction
  (rHat, sHat) <- createPreSignature alicePrivKey aliceTx adaptorPoint
  return (adaptorPoint, (rHat, sHat))

-- Bob completes the swap
bobComplete :: AdaptorPoint -> PreSignature -> IO Signature
bobComplete adaptorPoint alicePreSig = do
  -- Bob can verify the pre-signature
  -- Bob creates his signature (reveals his secret t)
  -- Alice can extract t from Bob's signature
  -- Alice uses t to complete her transaction
  ...
```

#### Learning Path

1. **Introduction**: Basic Schnorr signatures
2. **Linearity**: Demonstrate signature addition properties
3. **Adaptation**: Show how to "lock" signatures to secrets
4. **Extraction**: Demonstrate secret recovery
5. **Atomic Swaps**: Two-party protocol implementation
6. **Extensions**: Multi-party, threshold variants

### Option 2: Ed25519 with Ristretto255

**When to Choose**: If Ed25519 compatibility is essential

#### Approach

1. **Use Ristretto255 Library**: Abstracts away cofactor issues
2. **Implement Schnorr-style Adapter Signatures**: On the prime-order group
3. **Document Differences**: Explain why Ristretto255 is needed
4. **Compare with Raw Ed25519**: Show cofactor complications

#### Haskell Libraries

**Ed25519 Signing**:

- `ed25519` package: Hackage standard, bindings to ref10 SUPERCOP
- `eccrypto-ed25519-bindings`: Pure Haskell alternative

**Ristretto255**:

- Would need to identify Haskell bindings or use FFI to Rust/C libraries
- `libsodium` has Ristretto255 support

#### Implementation Outline

```haskell
-- Using a hypothetical Ristretto255 Haskell library
import Crypto.Ristretto255

-- Types are similar to Schnorr, but use Ristretto255 group
type RistrettoScalar = Ristretto255.Scalar
type RistrettoPoint = Ristretto255.Element

-- Adapter signature logic remains similar
-- But all operations use Ristretto255 group
```

### Option 3: Raw Ed25519 (ADVANCED ONLY)

**Warning**: Not recommended for educational implementation

#### Additional Requirements

1. **Explicit Cofactor Handling**

   ```haskell
   -- Must multiply by cofactor to clear small subgroup
   clearCofactor :: Point -> Point
   clearCofactor p = 8 .* p
   ```

2. **Careful Nonce Generation**
   - Ed25519's deterministic nonce: `r = H(h[32:64] || m)`
   - Adapter signatures need modified nonce generation
   - Risk of nonce reuse catastrophic failures

3. **Clamping Considerations**
   - Standard Ed25519 clamps private keys
   - May need unclamped variants for adapter secrets
   - Compatibility with standard verifiers problematic

4. **Security Proof Complexity**
   - All standard Schnorr proofs require modification
   - Small subgroup attacks must be explicitly prevented
   - Malleability issues must be addressed

#### When This Makes Sense

- Research project exploring Ed25519 specifics
- Need compatibility with existing Ed25519 infrastructure
- Advanced cryptography course covering curve subtleties

---

## Security Properties and Limitations

### Security Properties (Well-Constructed Adapter Signatures)

#### 1. Unforgeability

**Property**: An adversary cannot create valid signatures without the private key

**Schnorr/secp256k1**: Proven under discrete logarithm assumption in random oracle model

**Ed25519**: Proven with additional considerations:

- Must account for cofactor-8 group structure
- Clamping affects proof details
- Requires more careful analysis

#### 2. Pre-signature Adaptability

**Property**: A pre-signature can be adapted to a valid signature using the adaptor secret

**Requirement**: `Verify(pk, m, Adapt(preSign, t)) = True` where `t` is adaptor secret

**Both Schemes**: Property follows from linearity: `s = ŝ + t`

#### 3. Witness Extractability

**Property**: The adaptor secret can be extracted from pre-signature and signature

**Extraction**: `t = s - ŝ`

**Both Schemes**: Direct consequence of signature structure

#### 4. Privacy Properties

**Scriptless**: No on-chain indicator of adapter signature use

- Looks like standard signature to observers
- Atomic swap structure hidden

**Unlinkability**: Transactions on different chains not linkable

- Only parties involved know about the connection

### Limitations and Risks

#### Ed25519-Specific Risks

1. **Cofactor Attacks**
   - **Small Subgroup Confinement**: Attacker provides point in small subgroup
   - **Mitigation**: Multiply by cofactor, or use Ristretto255
   - **Impact**: Can lead to secret key recovery in some protocols

2. **Malleability**
   - **Issue**: Multiple valid signatures for same message
   - **Example**: Add low-order point to signature point R
   - **Historical**: Monero octuple-spend vulnerability
   - **Mitigation**: Check points are in prime-order subgroup

3. **Clamping Side Effects**
   - **Issue**: Restricts secret key space
   - **Impact**: May leak information about private keys
   - **Consideration**: Adapter secrets may need different handling

4. **Deterministic Nonce Complications**
   - **Standard Ed25519**: `r = H(privkey_hash || message)`
   - **Adapter Signatures**: Need modified nonce generation
   - **Risk**: Incorrect modification could leak private key

#### General Adapter Signature Risks

1. **Secret Reuse**
   - **Issue**: Same adaptor secret across multiple swaps
   - **Impact**: Linkability of transactions
   - **Mitigation**: Generate fresh adaptor secret per swap

2. **Timing Attacks**
   - **Issue**: Transaction timing reveals swap structure
   - **Impact**: Privacy loss
   - **Mitigation**: Random delays, batching

3. **Front-Running**
   - **Issue**: Adversary sees pre-signature, publishes adapted signature first
   - **Impact**: Steal the swap
   - **Mitigation**: Careful protocol design, timelocks

4. **Implementation Bugs**
   - **Issue**: Cryptographic code is subtle
   - **Impact**: Complete security compromise
   - **Mitigation**: Use audited libraries, formal verification

### Cross-Chain Complications

When implementing atomic swaps between different curves (e.g., Bitcoin secp256k1 ↔ Monero ed25519):

1. **Zero-Knowledge Proofs Required**
   - Must prove equal discrete logarithm across groups
   - Significantly increases complexity
   - Example: Farcaster protocol

2. **Different Security Assumptions**
   - Each curve has different security properties
   - Weakest link determines overall security
   - Careful analysis required

3. **Compatibility Challenges**
   - Different point encodings
   - Different signature formats
   - Bridge code increases attack surface

---

## Implementation Notes for Haskell

### Recommended Libraries

#### For Schnorr/secp256k1 Approach

```haskell
-- Elliptic curve operations
import Crypto.Secp256k1

-- Hashing
import Crypto.Hash (SHA256)
import qualified Crypto.Hash as Hash

-- Random number generation
import Crypto.Random (MonadRandom)
```

**Library**: `secp256k1-haskell`

- **Hackage**: https://hackage.haskell.org/package/secp256k1-haskell
- **Bindings**: To Bitcoin Core's libsecp256k1
- **Features**: Optimized, battle-tested implementation
- **Schnorr**: Supports Schnorr signatures (BIP340)

#### For Ed25519 Approach

```haskell
-- Ed25519 signatures
import Crypto.Sign.Ed25519
  ( PublicKey, SecretKey, Signature
  , createKeypair, sign, verify
  )

-- If implementing from scratch
import Crypto.ECC.Ed25519.Internal
```

**Library**: `ed25519`

- **Hackage**: https://hackage.haskell.org/package/ed25519
- **Implementation**: Bindings to ref10 SUPERCOP
- **Status**: Stable, widely used
- **Note**: Standard Ed25519, not adapted for adapter signatures

**Alternative**: `eccrypto` for pure Haskell implementation

#### For Ristretto255 (if choosing Option 2)

**Challenge**: Limited Haskell support currently

**Options**:

1. **FFI to Rust**: Use `ristretto255` crate via FFI
2. **FFI to C**: Use `libsodium` Ristretto255 support
3. **Pure Haskell**: Implement based on specification (significant effort)

### Code Architecture

#### Module Structure

```haskell
-- Core cryptographic primitives
module Crypto.Adapter.Primitives where
  -- Point and scalar operations
  -- Hash functions
  -- Random generation

-- Basic signatures (Schnorr or Ed25519)
module Crypto.Adapter.Signature where
  data Signature = Signature
    { sigR :: Point
    , sigS :: Scalar
    }

  sign :: PrivateKey -> Message -> IO Signature
  verify :: PublicKey -> Message -> Signature -> Bool

-- Adapter signature layer
module Crypto.Adapter.AdapterSignature where
  data PreSignature = PreSignature
    { preSigRHat :: Point
    , preSigSHat :: Scalar
    }

  createPreSignature :: PrivateKey -> Message -> AdaptorPoint -> IO PreSignature
  adaptSignature :: PreSignature -> AdaptorSecret -> Point -> Signature
  extractSecret :: PreSignature -> Signature -> Maybe AdaptorSecret

-- Atomic swap protocol
module Crypto.Adapter.AtomicSwap where
  data SwapState = Initiated | Locked | Completed | Refunded

  initiateSwap :: ...
  respondToSwap :: ...
  claimSwap :: ...
  refundSwap :: ...
```

#### Error Handling

```haskell
-- Comprehensive error types
data AdapterError
  = InvalidSignature
  | InvalidPreSignature
  | InvalidAdaptorPoint
  | SecretExtractionFailed
  | VerificationFailed String
  | PointNotOnCurve
  | ScalarOutOfRange
  deriving (Show, Eq)

-- Use Either or ExceptT for error handling
type AdapterResult a = Either AdapterError a

-- Or for more complex scenarios
newtype AdapterM a = AdapterM (ExceptT AdapterError IO a)
  deriving (Functor, Applicative, Monad, MonadError AdapterError, MonadIO)
```

#### Testing Strategy

```haskell
-- Property-based testing with QuickCheck
import Test.QuickCheck

-- Test basic signature properties
prop_signatureVerifies :: PrivateKey -> Message -> Property
prop_signatureVerifies privKey msg = monadicIO $ do
  sig <- run $ sign privKey msg
  let pubKey = derivePublicKey privKey
  assert $ verify pubKey msg sig

-- Test adapter signature properties
prop_secretExtraction :: PrivateKey -> Message -> AdaptorSecret -> Property
prop_secretExtraction privKey msg adaptorSecret = monadicIO $ do
  let adaptorPoint = adaptorSecret .* basePoint
  preSig <- run $ createPreSignature privKey msg adaptorPoint
  let sig = adaptSignature preSig adaptorSecret adaptorPoint
      extracted = extractSecret preSig sig
  assert $ extracted == Just adaptorSecret

-- Test atomic swap properties
prop_atomicSwapCompletes :: Property
prop_atomicSwapCompletes = monadicIO $ do
  -- Simulate full atomic swap
  -- Verify both parties can extract secrets
  -- Verify signatures are valid
  ...
```

### Performance Considerations

#### Optimization Points

1. **Point Operations**: Most expensive operations
   - Use optimized libraries (libsecp256k1, ref10)
   - Cache frequently used points
   - Batch operations when possible

2. **Hash Functions**: Second most expensive
   - Use efficient implementations
   - Consider hash function choice (SHA-256 vs SHA-512)

3. **Random Number Generation**: Ensure cryptographic quality
   - Use `crypto-random` or similar
   - Consider deterministic nonces where appropriate

#### Benchmarking

```haskell
import Criterion.Main

main :: IO ()
main = defaultMain
  [ bgroup "signatures"
    [ bench "sign" $ nfIO $ sign privKey msg
    , bench "verify" $ nf (verify pubKey msg) sig
    ]
  , bgroup "adapter"
    [ bench "createPreSignature" $ nfIO $ createPreSignature privKey msg adaptorPoint
    , bench "adaptSignature" $ nf (adaptSignature preSig) adaptorSecret
    , bench "extractSecret" $ nf (extractSecret preSig) sig
    ]
  ]
```

### Code Style Guidelines

Following the project's Haskell style:

```haskell
-- 2-space indentation
-- <80 character lines
-- Section-style comments for organization

--------------------------------------------------------------------------------
-- Main Functions ---------------------------------------------------------------

-- Public API functions

--------------------------------------------------------------------------------
-- Helper Functions -------------------------------------------------------------

-- Internal utilities at module bottom
```

### Security Checklist for Implementation

- [ ] Use constant-time comparisons for secrets
- [ ] Zeroize sensitive data after use
- [ ] Validate all points are on curve
- [ ] Check scalars are in valid range
- [ ] Verify nonces are never reused
- [ ] Implement proper error handling
- [ ] Add comprehensive logging (without leaking secrets)
- [ ] Use audited cryptographic libraries
- [ ] Write extensive property-based tests
- [ ] Consider formal verification for critical components

---

## References

### Academic Papers

1. **Zhu, Yixing, Huilin Li, Mengze Li, and Yong Yu.** "Adaptor signature based on randomized EdDSA in blockchain." _Digital Communications and Networks_ 11 (2025): 689-699.
   - DOI: 10.1016/j.dcan.2024.06.004
   - Published: June 12, 2024 (Open Access)
   - **Local Copy**: `pdf/1-s2.0-S2352864824000713-main.pdf`
   - Online: https://journal.hep.com.cn/dcn/EN/10.1016/j.dcan.2024.06.004
   - **Status**: ⭐ Primary reference - Complete algorithmic specification reviewed and integrated

2. **Aumayr, Lukas, et al.** "Two-Party Adaptor Signatures From Identification Schemes." _PKC 2021_.
   - IACR ePrint: https://eprint.iacr.org/2021/150

3. **Gugger, Joël.** "Bitcoin–Monero Cross-chain Atomic Swap." _IACR ePrint_ (2020).
   - https://eprint.iacr.org/2020/1126

4. **Thibault, Liam, et al.** "Foundations of Adaptor Signatures." _IACR ePrint_ (2024).
   - https://eprint.iacr.org/2024/1809

5. **Cremers, Cas, et al.** "The Provable Security of Ed25519: Theory and Practice." _IEEE S&P 2021_.
   - https://eprint.iacr.org/2020/823

6. **Brandão, Luís, et al.** "Notes on Threshold EdDSA/Schnorr Signatures." _NIST IR 8214B_ (2022).
   - https://nvlpubs.nist.gov/nistpubs/ir/2022/NIST.IR.8214B.ipd.pdf

### Specifications

7. **RFC 8032**: Edwards-Curve Digital Signature Algorithm (EdDSA)
   - https://datatracker.ietf.org/doc/html/rfc8032

8. **BIP 340**: Schnorr Signatures for secp256k1
   - https://github.com/bitcoin/bips/blob/master/bip-0340.mediawiki

9. **Ristretto255 Specification**: The ristretto255 and decaf448 Groups
   - https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-ristretto255-decaf448

### Implementations

10. **Farcaster Project**: Bitcoin-Monero Atomic Swaps
    - https://github.com/farcaster-project
    - Protocol: https://github.com/h4sh3d/xmr-btc-atomic-swap

11. **AthanorLabs**: ETH-XMR Atomic Swaps
    - https://github.com/AthanorLabs/atomic-swap

12. **Bitcoin Core libsecp256k1**: Schnorr signatures
    - https://github.com/bitcoin-core/secp256k1

### Educational Resources

13. **Conduition**: "The Riddles of Adaptor Signatures"
    - https://conduition.io/scriptless/adaptorsigs/

14. **Bitcoin Optech**: Adaptor Signatures Topic
    - https://bitcoinops.org/en/topics/adaptor-signatures/

15. **Blockstream Research**: Scriptless Scripts
    - https://github.com/BlockstreamResearch/scriptless-scripts/blob/master/md/atomic-swap.md

16. **Crypto Garage (Medium)**: Adaptor Signature Series
    - https://medium.com/crypto-garage/adaptor-signature-schnorr-signature-and-ecdsa-da0663c2adc4

17. **Tarilabs**: Introduction to Schnorr Signatures
    - https://tlu.tarilabs.com/cryptography/introduction-schnorr-signatures

### Haskell Libraries

18. **ed25519**: Ed25519 cryptographic signatures
    - https://hackage.haskell.org/package/ed25519

19. **secp256k1-haskell**: Bindings to libsecp256k1
    - https://hackage.haskell.org/package/secp256k1-haskell

20. **cryptonite**: Comprehensive cryptography library
    - https://hackage.haskell.org/package/cryptonite

### Community Resources

21. **Ristretto Group Website**
    - https://ristretto.group/

22. **Cardano Forum**: Atomic Swap Discussions
    - https://forum.cardano.org/t/alice-and-bob-doing-an-atomic-swap-breakdown/105684

23. **Monero CCS**: Atomic Swap Funding Proposals
    - https://ccs.getmonero.org/proposals/h4sh3d-atomic-swap-implementation.html

---

## Conclusion and Recommendations

### Summary of Findings

1. **Ed25519 Adapter Signatures Are Possible**: Both theoretical work (2024 paper) and production implementations (Farcaster) demonstrate feasibility.

2. **Significant Complexity**: Ed25519's cofactor-8, clamping, and other specifics make implementation more complex than Schnorr on secp256k1.

3. **Cross-Chain Requires Additional Primitives**: When swapping between different curves, zero-knowledge proofs for discrete log equality are necessary.

4. **Ristretto255 Simplifies**: Using Ristretto255 abstraction eliminates cofactor complications while maintaining Ed25519 compatibility.

### For Educational Implementation

**Primary Recommendation**: Implement Schnorr adapter signatures on secp256k1

**Reasoning**:

- Clearer security model for learners
- More accessible mathematical proofs
- Abundant reference materials
- Direct applicability to Bitcoin/Lightning

**Learning Progression**:

1. Start with basic Schnorr signatures
2. Add adapter signature layer
3. Implement atomic swap protocol
4. Discuss Ed25519 differences as advanced topic
5. (Optional) Implement Ed25519 variant with Ristretto255

### For Production Implementation

**If Using Ed25519**:

1. Consider Ristretto255 to avoid cofactor issues
2. Reference Farcaster implementation for cross-chain scenarios
3. Use audited cryptographic libraries
4. Invest in formal security analysis
5. Implement comprehensive testing

**If Using Schnorr/secp256k1**:

1. Use Bitcoin Core's libsecp256k1
2. Follow BIP 340 specification
3. Reference Lightning Network implementations
4. Extensive property-based testing

### Future Research Directions

1. **Formal Verification**: Mechanized proofs of Ed25519 adapter signature security
2. **Threshold Variants**: Multi-party Ed25519 adapter signatures
3. **Post-Quantum**: Lattice-based adapter signature constructions
4. **Optimizations**: Batch verification for adapter signatures
5. **Standards**: Formal specification for EdDSA adapter signatures

### Open Questions

1. What is the complete construction from the 2024 Zhu et al. paper?
2. How do performance characteristics compare: Ed25519 vs Schnorr adapter signatures?
3. Can Ristretto255-based adapter signatures be made compatible with standard Ed25519 verifiers?
4. What are the formal security reductions for Ed25519 adapter signatures?
5. Are there efficient batch verification algorithms for adapter signatures?

---

## Appendix A: Basic Schnorr Signature Primer

For readers unfamiliar with Schnorr signatures, here's a quick primer:

### Setup

- **Curve**: Elliptic curve with base point G and prime order q
- **Private Key**: Random scalar a ∈ [1, q-1]
- **Public Key**: Point A = a·G

### Signing Algorithm

1. Generate random nonce k ∈ [1, q-1]
2. Compute nonce point R = k·G
3. Compute challenge c = H(R || A || m) where m is message
4. Compute signature scalar s = k + c·a
5. Signature is (R, s)

### Verification Algorithm

1. Parse signature as (R, s)
2. Compute challenge c = H(R || A || m)
3. Check: s·G = R + c·A
4. Accept if equal, reject otherwise

### Why Verification Works

```
s·G = (k + c·a)·G           [by signature construction]
    = k·G + c·a·G           [by point addition]
    = R + c·A               [by definition of R and A]
```

### Linearity Property

If you have two signatures (R₁, s₁) and (R₂, s₂):

```
Combined signature: (R₁ + R₂, s₁ + s₂)
Verifies for combined public key: A₁ + A₂
```

This linearity is the foundation of adapter signatures.

---

## Appendix B: Glossary

**Adapter Signature / Adaptor Signature**: A signature that's "locked" to a secret value; revealing the signature reveals the secret, and vice versa. Also called verifiably encrypted signature (VES).

**Atomic Swap**: A protocol for exchanging assets between two parties where either both exchanges complete or neither does (atomicity).

**Clamping**: Ed25519 convention of forcing private key bits into specific patterns to ensure the key is a multiple of 8 and in a specific range.

**Cofactor**: Small integer h such that the curve has order h × l where l is a large prime. For Ed25519, h = 8. For secp256k1, h = 1.

**Edwards Curve**: Elliptic curve in Edwards form: ax² + y² = 1 + dx²y². Used by Ed25519.

**Pre-signature**: The "encrypted" signature in an adapter signature scheme, before the adaptor secret is applied.

**Prime-Order Group**: Group whose order is a prime number. Simplifies security proofs.

**Ristretto255**: Abstraction layer over Curve25519 that provides a prime-order group interface.

**Schnorr Signature**: Linear signature scheme based on discrete logarithm problem. Foundation for adapter signatures.

**Scriptless Scripts**: Smart contract functionality implemented using cryptographic protocols (like adapter signatures) rather than on-chain scripts.

**secp256k1**: Elliptic curve used by Bitcoin. Has cofactor 1 (prime-order).

**Witness Extractability**: Property that the adaptor secret can be extracted from a pre-signature and completed signature pair.

**Zero-Knowledge Proof**: Cryptographic proof that allows proving a statement is true without revealing why it's true.

---

**Document Version**: 2.0
**Last Updated**: 2025-11-14 (Zhu et al. 2024 paper fully integrated)
**Next Review**: After completing initial implementation
**Status**: ✅ Research complete with full specification - READY FOR IMPLEMENTATION
