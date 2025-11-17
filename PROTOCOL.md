# Atomic Swap Protocol using Adapter Signatures

## Overview

This document describes the adapter signature-based atomic swap protocol implemented in this tutorial. The protocol enables trustless, atomic exchange of digital assets across two independent blockchains without requiring hash-timelocked contracts (HTLCs) or on-chain scripting.

## Protocol Flow Diagram

```mermaid
---
config:
  theme: dark
---
sequenceDiagram
    autonumber

    box Swap Parties
        participant Alice
        participant Bob
    end

    box Blockchains
        participant ChainA as Blockchain A
        participant ChainB as Blockchain B
    end

    Note over Alice,ChainB: Phase 1: Setup and Key Exchange

    par Key Generation
        Alice->>+Alice: Generate keypair<br/>(sk0_A, sk1_A, pk_A)
        Alice->>-Alice: Ready
    and
        Bob->>+Bob: Generate keypair<br/>(sk0_B, sk1_B, pk_B)
        Bob->>-Bob: Ready
    end

    Alice->>Bob: Send pk_A
    Bob->>Alice: Send pk_B

    Note over Alice,Bob: Agree on swap terms:<br/>Alice: 10 ChainA tokens<br/>Bob: 5 ChainB tokens

    Alice->>+Alice: Generate adapter secret y ∈ ℤ_q*
    Alice->>Alice: Compute adapter point Y = y · B
    Alice->>-Alice: Generate NIZK proof π = Prove_zk(Y, y)

    Alice->>Bob: Send (Y, π)

    Bob->>+Bob: Verify NIZK proof
    Bob->>-Bob: Verify_zk(Y, π) = 1 ✓

    Note over Alice,ChainB: Phase 2: Adapted Pre-Signature Creation

    Alice->>+ChainA: Query UTXO
    ChainA-->>-Alice: UTXO confirmed (10 tokens)

    Alice->>+Alice: Build Tx_A: 10 tokens → pk_B
    Alice->>Alice: Create adapted pre-signature σ_tilde_A:<br/>• r_A = H2(sk1_A ‖ Tx_A ‖ k_A)<br/>• R_sign_A = r_A·B + Y<br/>• sig_tilde_A = r_A + h_A·sk0_A
    Alice->>-Alice: Pre-signature ready

    Alice->>Bob: Send (Tx_A, σ_tilde_A)

    critical Bob verifies Alice's pre-signature
        Bob->>+Bob: Compute R'_A = R_sign_A - Y
        Bob->>Bob: Compute h_A = H2(R_sign_A ‖ pk_A ‖ Tx_A)
        Bob->>-Bob: Verify: sig_tilde_A·B = R'_A + h_A·pk_A ✓
    option Verification fails
        Bob->>Alice: Abort protocol
    end

    Bob->>+ChainB: Query UTXO
    ChainB-->>-Bob: UTXO confirmed (5 tokens)

    Bob->>+Bob: Build Tx_B: 5 tokens → pk_A
    Bob->>Bob: Create adapted pre-signature σ_tilde_B:<br/>• r_B = H2(sk1_B ‖ Tx_B ‖ k_B)<br/>• R_sign_B = r_B·B + Y (same Y!)<br/>• sig_tilde_B = r_B + h_B·sk0_B
    Bob->>-Bob: Pre-signature ready

    Bob->>Alice: Send (Tx_B, σ_tilde_B)

    Note over Alice,ChainB: Phase 3: Pre-Signature Verification

    critical Alice verifies Bob's pre-signature
        Alice->>+Alice: Compute R'_B = R_sign_B - Y
        Alice->>Alice: Compute h_B = H2(R_sign_B ‖ pk_B ‖ Tx_B)
        Alice->>-Alice: Verify: sig_tilde_B·B = R'_B + h_B·pk_B ✓
    option Verification fails
        Alice->>Bob: Abort protocol
    end

    Note over Alice,Bob: ✓ Both parties have verified adapted pre-signatures<br/>⚠ Neither can execute yet (both need y)

    Note over Alice,ChainB: Phase 4: Atomic Execution

    Alice->>+Alice: Complete signature: sig_A = sig_tilde_A + y
    Alice->>-ChainA: Publish Tx_A with σ_A = (R_sign_A, sig_A)

    activate ChainA
    ChainA->>ChainA: Verify: sig_A·B = R_sign_A + h_A·pk_A ✓
    ChainA-->>Alice: ✓ Transaction confirmed<br/>Alice receives 5 tokens
    deactivate ChainA

    Bob->>ChainA: Observe published Tx_A

    critical Bob extracts adapter secret
        Bob->>+Bob: Extract: y = sig_A - sig_tilde_A
        Bob->>-Bob: Verify: Y = y·B ✓
    end

    Bob->>+Bob: Complete signature: sig_B = sig_tilde_B + y
    Bob->>-ChainB: Publish Tx_B with σ_B = (R_sign_B, sig_B)

    activate ChainB
    ChainB->>ChainB: Verify: sig_B·B = R_sign_B + h_B·pk_B ✓
    ChainB-->>Bob: ✓ Transaction confirmed<br/>Bob receives 10 tokens
    deactivate ChainB

    Note over Alice,ChainB: 🎉 Swap Complete!<br/>Both parties have exchanged assets atomically
```

## Cryptographic Foundation

### Choice of Signature Scheme

Based on comprehensive research (see `research/2025-11-14-ed25519-adapter-signatures.md` and `research/2025-11-14-two-party-atomic-swap-protocol.md`), this implementation uses:

**Primary**: Randomized EdDSA (rEdDSA) adapter signatures on Ed25519
**Rationale**:

- Complete specification from Zhu et al. (2024) with formal security proofs
- Direct Cardano/Monero compatibility
- Better performance than ECDSA alternatives (33% smaller signatures)
- Clean mathematical structure ideal for education

**Reference**: "Adaptor signature based on randomized EdDSA in blockchain" (Zhu et al., Digital Communications and Networks, 2024)

### Adapter Signature Concept

An adapter signature is a pre-signature that:

1. Can be verified as "almost valid"
2. Requires a secret value `t` to complete
3. Reveals `t` when completed and published

**Key Property**: If Alice can complete her signature only by learning Bob's secret `t`, and Bob reveals `t` by publishing his transaction, then Alice can atomically complete her transaction.

## Protocol Phases (V1 - Simplified)

**Note**: This is the simplified version for v1 focusing on adapter signature mechanics. Future versions will add multisig locking and refund timelocks (see IMPLEMENTATION-PLAN.md).

### Phase 1: Setup and Key Exchange

```
1. Alice generates rEdDSA keypair:
   - Private: (sk0_A, sk1_A)
   - Public: pk_A = sk0_A · B

2. Bob generates rEdDSA keypair:
   - Private: (sk0_B, sk1_B)
   - Public: pk_B = sk0_B · B

3. Alice and Bob exchange public keys via TMVar

4. Swap terms (hardcoded for v1):
   - Alice offers: 10 ChainA tokens
   - Bob offers: 5 ChainB tokens

5. Alice generates adapter secret: y ∈ ℤ_q* (random scalar)

6. Alice computes adapter point: Y = y · B

7. Alice generates NIZK proof: π = Prove_zk(Y, y)
   Proves: "I know y such that Y = y·B"

8. Alice sends (Y, π) to Bob via TMVar

9. Bob verifies NIZK proof: Verify_zk(Y, π) = 1
```

### Phase 2: Adapted Pre-Signature Creation

**Alice creates adapted pre-signature** (following Zhu et al. 2024):

```
10. Alice queries her UTXO on ChainA (has 10 tokens)

11. Alice builds transaction Tx_A:
    Inputs:  Alice's UTXO (10 tokens)
    Outputs: 10 tokens to Bob's public key pk_B

12. Alice creates adapted pre-signature for Tx_A:
    a. Select random k_A ∈ ℤ_q*
    b. Compute r_A = H2(sk1_A || Tx_A || k_A) mod q
    c. Compute R_pre_A = r_A · B
    d. Compute adapted nonce: R_sign_A = R_pre_A + Y  ← KEY: Add adapter point!
    e. Compute challenge: h_A = H2(R_sign_A || pk_A || Tx_A)
    f. Compute pre-signature scalar: sig_tilde_A = r_A + h_A · sk0_A mod q
       Note: Does NOT include y!
    g. Output: σ_tilde_A = (sig_tilde_A, R_sign_A, π)

13. Alice sends (Tx_A, σ_tilde_A) to Bob via TMVar
```

**Bob creates adapted pre-signature**:

```
14. Bob receives and verifies Alice's adapted pre-signature:
    a. Parse σ_tilde_A = (sig_tilde_A, R_sign_A, π)
    b. Compute R'_A = R_sign_A - Y  ← Remove adapter point
    c. Compute h_A = H2(R_sign_A || pk_A || Tx_A)
    d. Verify: sig_tilde_A · B = R'_A + h_A · pk_A
    e. Verify: Verify_zk(Y, π) = 1
    f. If valid, continue; else abort

15. Bob queries his UTXO on ChainB (has 5 tokens)

16. Bob builds transaction Tx_B:
    Inputs:  Bob's UTXO (5 tokens)
    Outputs: 5 tokens to Alice's public key pk_A

17. Bob creates adapted pre-signature for Tx_B using Alice's Y:
    a. Select random k_B ∈ ℤ_q*
    b. Compute r_B = H2(sk1_B || Tx_B || k_B) mod q
    c. Compute R_pre_B = r_B · B
    d. Compute adapted nonce: R_sign_B = R_pre_B + Y  ← Same Y from Alice!
    e. Compute challenge: h_B = H2(R_sign_B || pk_B || Tx_B)
    f. Compute pre-signature scalar: sig_tilde_B = r_B + h_B · sk0_B mod q
    g. Output: σ_tilde_B = (sig_tilde_B, R_sign_B, π)

18. Bob sends (Tx_B, σ_tilde_B) to Alice via TMVar
```

### Phase 3: Pre-Signature Verification

```
19. Alice receives (Tx_B, σ_tilde_B) from Bob

20. Alice verifies Bob's adapted pre-signature:
    a. Parse σ_tilde_B = (sig_tilde_B, R_sign_B, π)
    b. Compute R'_B = R_sign_B - Y
    c. Compute h_B = H2(R_sign_B || pk_B || Tx_B)
    d. Verify: sig_tilde_B · B = R'_B + h_B · pk_B
    e. Verify: Verify_zk(Y, π) = 1
    f. If valid, continue; else abort

Both parties now have verified adapted pre-signatures.
Neither can execute their transaction yet (both need y to complete).
```

### Phase 4: Atomic Execution

**Alice publishes first** (initiator reveals secret):

```
21. Alice completes her signature:
    sig_A = sig_tilde_A + y  ← Add adapter secret!

22. Alice creates complete signature: σ_A = (R_sign_A, sig_A)

23. Alice adds signature to Tx_A and publishes to ChainA

24. ChainA verifies signature:
    sig_A · B = R_sign_A + h_A · pk_A  ✓
    (Works because: sig_A · B = (sig_tilde_A + y) · B
                                = sig_tilde_A · B + y · B
                                = R'_A + h_A · pk_A + Y
                                = (R_sign_A - Y) + h_A · pk_A + Y
                                = R_sign_A + h_A · pk_A)

25. ChainA confirms transaction - Alice receives Bob's funds
```

**Bob extracts secret and publishes**:

```
26. Bob observes Tx_A published on ChainA

27. Bob extracts Alice's complete signature σ_A = (R_sign_A, sig_A)

28. Bob computes adapter secret:
    y = sig_A - sig_tilde_A  ← THE MAGIC!

29. Bob verifies extraction: Y = y · B  (should match Alice's commitment)

30. Bob completes his signature:
    sig_B = sig_tilde_B + y  ← Use extracted secret!

31. Bob creates complete signature: σ_B = (R_sign_B, sig_B)

32. Bob adds signature to Tx_B and publishes to ChainB

33. ChainB verifies signature:
    sig_B · B = R_sign_B + h_B · pk_B  ✓

34. ChainB confirms transaction - Bob receives Alice's funds

35. Swap complete! Both parties have swapped assets.
```

### Why This Works (Atomicity Proof)

**Cryptographic Linkage**:

- Both adapted pre-signatures use the **same Y** value
- Alice's pre-sig: `sig_tilde_A = r_A + h_A · sk0_A` with nonce `R_sign_A = r_A·B + Y`
- Bob's pre-sig: `sig_tilde_B = r_B + h_B · sk0_B` with nonce `R_sign_B = r_B·B + Y`

**Forward Direction (Alice → Bob)**:

- Alice cannot publish valid signature without adding `y`
- When Alice publishes `sig_A = sig_tilde_A + y`, the value `y` becomes extractable
- Bob computes: `y = sig_A - sig_tilde_A` (he has both values!)

**Reverse Direction (Bob cannot proceed without Alice)**:

- Bob cannot compute `sig_B` without knowing `y`
- Bob cannot learn `y` until Alice publishes `sig_A`
- If Alice never publishes, Bob never learns `y`, neither swap completes

**Atomicity**: Alice cannot claim Bob's funds without revealing `y`, and once `y` is revealed, Bob can always claim Alice's funds.

## Security Properties

### Atomicity

**Property**: Either both transactions complete, or neither completes.

**Guarantee**: Once Bob publishes his transaction (revealing `t`), Alice can extract `t` and complete hers. If Bob never publishes, Alice never reveals her transaction, so neither side loses funds.

### Fairness

**Property**: No party can gain an advantage by deviating from the protocol.

**Guarantee**:

- Bob cannot take Alice's funds without revealing `t`
- Once Bob reveals `t`, Alice is guaranteed to learn it
- If either party aborts early, no funds are exchanged

### Privacy

**Property**: The atomic swap leaves no identifiable trace on either blockchain.

**Advantage over HTLCs**:

- No hash preimages revealed on-chain
- Transactions look like regular transfers
- No obvious link between the two chains
- "Scriptless scripts" - no smart contract code visible

### Non-interactivity

**Property**: After initial setup, no further communication needed.

**Benefit**: Protocol continues even if communication channel is disrupted after signature exchange.

## Error Handling

### Invalid Adapted Signature

If verification fails in Phase 3:

```
Error: Adapted signature verification failed
Action: Abort protocol, no funds committed
```

### Insufficient Funds

If UTXO balance check fails in Phase 2:

```
Error: Insufficient balance for swap
Action: Abort protocol before signature creation
```

### Timeout (Future Extension)

In production systems, add timelocks:

```
Timelock: If Bob doesn't publish within N blocks, Alice can reclaim funds
Implementation: Requires additional scripting or refund transactions
Status: Out of scope for v1 (happy path only)
```

## Comparison with Hash-Timelocked Contracts (HTLCs)

| Property               | Adapter Signatures        | HTLCs                        |
| ---------------------- | ------------------------- | ---------------------------- |
| **Privacy**            | High (no visible link)    | Low (hash preimage on-chain) |
| **Script Required**    | No                        | Yes                          |
| **On-chain Footprint** | Regular transaction       | Specialized contract         |
| **Complexity**         | Higher (cryptographic)    | Lower (hash comparison)      |
| **Flexibility**        | More (scriptless scripts) | Less (fixed hash lock)       |

## Implementation Notes

### UTXO Model

Transactions consume inputs (UTXOs) and create outputs:

```haskell
data Transaction = Transaction
  { txInputs :: [UTXO]
  , txOutputs :: [Output]
  , txSignatures :: [Signature]
  }

data UTXO = UTXO
  { utxoTxId :: TxId
  , utxoIndex :: Word32
  , utxoAmount :: Word64
  , utxoOwner :: PublicKey
  }
```

### Signature Verification

Standard Schnorr verification:

```
s·G = R + H(R || A || m)·A
```

Adapted signature verification (pre-signature):

```
ŝ·G = R + H(R || A || m)·A - T
```

Where:

- `s`, `ŝ` = signature scalar (completed / pre-signature)
- `G` = generator point
- `R` = nonce point
- `A` = public key
- `m` = message (transaction data)
- `T` = adapter point

### Thread Communication

Alice and Bob run as separate threads communicating via STM queue:

```haskell
data Message
  = PublicKeyExchange PublicKey
  | AdapterPointMsg Point
  | TransactionProposal Transaction AdaptedSignature
  | SwapComplete
```

### Logging

Verbose logging shows every step:

```
[Alice] Generated keypair
[Alice]   Public key: 02a1b2c3...
[Bob] Received public key from Alice
[Bob] Generated adapter point T
[Alice] Creating adapted signature for ChainA transaction
[Alice]   Nonce R: 03d4e5f6...
[Alice]   Pre-signature ŝ: 7f8a9b...
```

## References

- **Scriptless Scripts**: Andrew Poelstra, 2017
- **Adaptor Signatures**: Lloyd Fournier, 2019
- **Farcaster**: Bitcoin-Monero atomic swap implementation
- See full bibliography in `research/2025-11-14-ed25519-adapter-signatures.md`

---

**Status**: Initial version for v0.1.0 (happy path only)
**Next**: Add timelock refunds, error recovery, Ed25519 support
