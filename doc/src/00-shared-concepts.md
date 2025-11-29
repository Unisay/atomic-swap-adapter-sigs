# Shared Concepts: Cardano-Monero Atomic Swap

> **Purpose**: Single source of truth for terminology, notation, and common phases
> **Prerequisite**: Basic understanding of cryptographic signatures
> **Reference**: COMIT XMR-BTC RFC, Farcaster RFC-08, adapted for Cardano-Monero

## Terminology

### Parties

| Party            | Has | Wants | Role                                                              |
| ---------------- | --- | ----- | ----------------------------------------------------------------- |
| **Monero User**  | XMR | ADA   | Generates adapter secret `y`, locks on scriptless chain           |
| **Cardano User** | ADA | XMR   | Locks first on scripted chain, holds Monero key share `s_cardano` |

### Key Asymmetry

The **Monero User** controls swap timing because they hold the adapter secret `y`:

- Monero User can complete the swap anytime by revealing `y`
- Cardano User must wait for `y` to be revealed
- This asymmetry is balanced by lock order and punish mechanism

---

## Cryptographic Primitives

### Adapter Signatures (Happy Path)

| Term                  | Symbol      | Description                                         |
| --------------------- | ----------- | --------------------------------------------------- |
| **Adapter Secret**    | `y`         | Random 32-byte scalar, known only to Monero User    |
| **Adapter Point**     | `Y = y·B`   | Public commitment to adapter secret (Ed25519 point) |
| **Pre-signature**     | `σ̃`         | Incomplete signature requiring `y` to complete      |
| **Adapted Signature** | `σ = σ̃ + y` | Complete signature that reveals `y`                 |
| **Secret Extraction** | `y = σ - σ̃` | Anyone with `σ̃` can extract `y` from `σ`            |

### Monero Key Shares (Refund Path)

| Term                         | Symbol                           | Description                                       |
| ---------------------------- | -------------------------------- | ------------------------------------------------- |
| **Cardano User's key share** | `s_cardano`                      | Private scalar for Monero 2-of-2 multisig         |
| **Monero User's key share**  | `s_monero`                       | Private scalar for Monero 2-of-2 multisig         |
| **Combined spend key**       | `s_total = s_cardano + s_monero` | Required to spend from Monero multisig            |
| **Refund adaptor signature** | `σ̃_refund`                       | Encrypted with `S_cardano`, given to Cardano User |

### Key Properties

**Happy Path (Buy):**

```
If Monero User publishes σ = σ̃ + y on Cardano blockchain,
Then Cardano User can compute y = σ - σ̃ (they have σ̃ from setup)
Then Cardano User can claim XMR on Monero using y
```

**Refund Path:**

```
If Cardano User publishes Refund with σ_refund = σ̃_refund + s_cardano,
Then Monero User can compute s_cardano = σ_refund - σ̃_refund
Then Monero User can recover XMR using s_total = s_cardano + s_monero
```

This creates **atomic linkage** in both paths.

> **Security Note (Nonce Generation)**: Adapter signatures require randomized nonces.
> Using deterministic nonces (standard EdDSA) would leak the adapter secret through
> related-nonce attacks. Always use rEdDSA with fresh randomness: `r = H2(sk1 || msg || k)`
> where `k` is a fresh 32-byte random value.

---

## Blockchain Characteristics

| Chain       | Type                   | Script Capability      | Timelock Support | Curve   |
| ----------- | ---------------------- | ---------------------- | ---------------- | ------- |
| **Cardano** | Scripted (Arbitrating) | Full Plutus validators | Yes (slot-based) | Ed25519 |
| **Monero**  | Scriptless (Accordant) | 2-of-2 multisig only   | No               | Ed25519 |

### Implications

- **Cardano** can enforce complex spending conditions (Buy/Cancel/Refund/Punish paths)
- **Monero** can only enforce "both parties agree" via 2-of-2 multisig
- **Same curve** (Ed25519) means no cross-curve DLEQ proofs needed

### Monero Implementation Details

| Aspect                   | Requirement                                     | Rationale                               |
| ------------------------ | ----------------------------------------------- | --------------------------------------- |
| **Confirmations**        | 10+ blocks (~20 min)                            | Reorg resistance for irreversible locks |
| **View Key**             | Shared with counterparty                        | Allows monitoring of lock transaction   |
| **Key Share Validation** | Verify `S_cardano` corresponds to claimed share | Prevents malicious key substitution     |
| **Multisig Address**     | Derive from `S_total = S_cardano + S_monero`    | Standard 2-of-2 construction            |

**View Key Distribution**: During setup, Monero User shares view key with Cardano User
so Cardano User can independently verify XMR lock without trusting Monero User's claims.

---

## Lock Order (Critical)

> **Rule**: Scripted chain locks FIRST, scriptless chain locks SECOND

```
1. Cardano User locks ADA on Cardano (scripted)
   └─ If this fails/never happens, Monero User simply doesn't lock

2. Monero User locks XMR on Monero (scriptless)
   └─ Only after verifying Cardano lock is confirmed
```

### Rationale

- Party on scripted chain takes more risk (funds locked with timelocks)
- Party on scriptless chain can always refuse to lock if scripted party doesn't lock first
- This prevents "Cardano User never locks" grief vector

---

## Timelock Parameters

| Timelock          | Symbol | Purpose                                        | Typical Value          |
| ----------------- | ------ | ---------------------------------------------- | ---------------------- |
| Lock confirmation | `t₀`   | Time for lock tx to confirm                    | +6 blocks (~2 min)     |
| Cancel window     | `t₁`   | After t₁, Cardano User can Cancel              | +12 hours              |
| Punish window     | `t₂`   | After t₂ (from Cancel), Monero User can Punish | +24 hours after Cancel |

### Timeline

```
Lock ──────────────────────┬─────────────────────────────────────────────────►
                           │
                          t₁
                     Cancel opens
                           │
                           ▼
                    ┌──────────────┐
                    │ Cancel UTXO  │──────────┬────────────────────────────►
                    └──────────────┘          │
                                             t₂ (measured from Cancel)
                                        Punish opens

Before t₁: Only Buy transaction available (from Lock UTXO)
After t₁:  Cancel available (moves ADA to Cancel UTXO)
After t₂:  Punish available (from Cancel UTXO)
```

**Critical**: `t₂` is measured from when Cancel is published, NOT from Lock.
This gives Cardano User time to Refund before Punish becomes available.

---

## Four-Transaction Pattern (Cardano Side)

The Cardano Plutus scripts support four transactions across two UTXOs:

### Lock UTXO (Created by Lock Transaction)

#### 1. Buy Transaction (Happy Path)

- **Who**: Monero User
- **When**: Anytime before Cancel is published
- **Requires**: Adapted signature `σ = σ̃ + y` revealing adapter secret `y`
- **Result**: Monero User gets ADA, `y` is public, Cardano User can claim XMR
- **Spends**: Lock UTXO

#### 2. Cancel Transaction (Abort Initiation)

- **Who**: Cardano User
- **When**: After timelock `t₁` expires
- **Requires**: Cardano User's signature (NO secrets revealed)
- **Result**: Commits to abort path, creates Cancel UTXO
- **Spends**: Lock UTXO
- **Creates**: Cancel UTXO

### Cancel UTXO (Created by Cancel Transaction)

#### 3. Refund Transaction (Abort Completion)

- **Who**: Cardano User
- **When**: Anytime after Cancel (ideally before `t₂`)
- **Requires**: Adapted signature `σ_refund = σ̃_refund + s_cardano` revealing Monero key share
- **Result**: Cardano User gets ADA back, Monero User can recover XMR
- **Spends**: Cancel UTXO

#### 4. Punish Transaction (Grief Penalty)

- **Who**: Monero User
- **When**: After timelock `t₂` expires (measured from Cancel)
- **Requires**: Monero User's signature
- **Result**: Monero User takes ALL locked ADA as penalty
- **Spends**: Cancel UTXO

### Why Two UTXOs?

**Cancel protects Cardano User from Punish race:**

Without Cancel, after `t₂` there would be a three-way race (Buy vs Refund vs Punish).
A malicious Monero User could race Punish and potentially win even if Cardano User
was about to Refund.

With Cancel:

1. Cardano User publishes Cancel between `t₁` and `t₂`
2. Buy path is eliminated (Lock UTXO spent)
3. Only Refund vs Punish race remains (from Cancel UTXO)
4. Cardano User has safe window to commit to abort before Punish is available

---

## Refund Mechanism

> **Key Insight**: The REFUND transaction reveals `s_cardano`, NOT Cancel.
> This separation is the critical security property.

### How Monero Refund Works

1. Cardano User publishes Cancel on Cardano (NO secrets revealed)
2. Cardano User publishes Refund on Cardano
3. Refund tx contains adaptor signature: `σ_refund = σ̃_refund + s_cardano`
4. Monero User observes Refund tx on Cardano
5. Monero User extracts Cardano User's private key share: `s_cardano = σ_refund - σ̃_refund`
6. Monero User computes full spend key: `s_total = s_cardano + s_monero`
7. Monero User spends 2-of-2 multisig unilaterally, recovers XMR

### Why This Prevents "Free Option" Attack

Because Cancel reveals nothing, Monero User cannot recover XMR until Cardano User Refunds:

```
Cancel → NO secrets revealed → ADA in Cancel UTXO
Refund → s_cardano revealed → Monero User can recover XMR
                            → ADA returned to Cardano User
                            → Cancel UTXO spent, no Punish possible
```

### Outcomes After Cancel

| Cardano User Action | Cardano User Gets        | Monero User Gets                   |
| ------------------- | ------------------------ | ---------------------------------- |
| Publishes Refund    | ADA back                 | Extracts `s_cardano`, recovers XMR |
| Doesn't Refund      | **Loses ADA** (Punished) | Gets ADA, **XMR locked forever**   |

**Monero User cannot get both XMR and ADA** — this eliminates the "free option".

---

## Economic Incentives

### Honest Behavior Payoffs

| Scenario        | Cardano User     | Monero User      |
| --------------- | ---------------- | ---------------- |
| Successful swap | -100 ADA, +1 XMR | +100 ADA, -1 XMR |
| Cancel + Refund | ±0               | ±0               |

### Grief Attempt Payoffs

| Grief Attempt                  | Cardano User            | Monero User                           |
| ------------------------------ | ----------------------- | ------------------------------------- |
| Monero User never reveals `y`  | ±0 (via Refund)         | ±0 (XMR recovered)                    |
| Cardano User refuses to Refund | **-100 ADA** (Punished) | +100 ADA, **-1 XMR** (locked forever) |

### Key Insight

Griefing is economically irrational:

- **Monero User grief** (don't Buy): No profit, both parties refund
- **Cardano User grief** (don't Refund): Loses entire swap amount, Monero User compensated with ADA

Note: If Cardano User is Punished, the XMR remains **locked forever** in the 2-of-2 multisig
(neither party has both key shares). Monero User accepts ADA as compensation.

---

## Common Phases (Reusable Across All Flows)

### Phase 1: Setup (Off-Chain)

```
1. Both parties generate Monero key shares:
   - Cardano User: s_cardano, S_cardano = s_cardano·G
   - Monero User: s_monero, S_monero = s_monero·G
   - Combined public key: S_total = S_cardano + S_monero

2. Monero User generates adapter secret:
   - y (32-byte random scalar)
   - Y = y·B (public commitment)

3. Both parties exchange:
   - Cardano public keys (for ADA transactions)
   - Monero public key shares (S_cardano, S_monero)
   - Adapter point Y

4. Pre-signatures created:
   - Buy: Cardano User creates σ̃_buy adapted by Y (for Monero User)
   - Refund: Monero User creates σ̃_refund adapted by S_cardano (for Cardano User)
   - Cancel: Both sign (standard multisig, no adaptor)
   - Punish: Cardano User pre-signs (for Monero User to use if needed)

5. Both parties verify all pre-signatures
```

#### Setup Validation Checklist

Before proceeding to Phase 2, both parties MUST verify:

- [ ] **Adapter point**: Y is valid Ed25519 point (on curve, not identity)
- [ ] **Key shares**: S_cardano and S_monero are valid Ed25519 points
- [ ] **NIZK proofs**: Discrete log proofs for Y, S_cardano, S_monero verify correctly
- [ ] **Pre-signatures**: All adapted pre-signatures verify against correct public keys
- [ ] **Timelock parameters**: t₁ and t₂ values are acceptable (sufficient safety margins)
- [ ] **Amounts**: Swap amounts match agreed terms

**Failure to validate can result in fund loss or protocol deadlock.**

### Phase 2: Lock (On-Chain)

```
1. Cardano User locks ADA on Cardano (scripted chain first)
   - Creates Lock UTXO with Plutus script
   - Datum stores: Y, S_cardano, pk_cardano, pk_monero, t₁
   - Script paths: Buy (anytime), Cancel (after t₁)

2. Monero User verifies Cardano lock:
   - Confirms Lock UTXO exists with correct parameters
   - Waits for sufficient confirmations

3. Monero User locks XMR on Monero (scriptless chain second)
   - Creates 2-of-2 multisig address with S_total = S_cardano + S_monero
   - Funds multisig with swap amount
```

### Phase 3: Resolution (Scenario-Specific)

See individual flow documents for scenario-specific resolution paths.

---

## Security Properties

### Atomicity

Either both parties receive their funds, or neither does.
Partial completion is cryptographically impossible due to adaptor signature linkage.

### Liveness

Protocol always terminates within bounded time (max Cancel + t₂ + confirmation time).

### Fairness

Neither party can gain advantage through deviation from protocol.
The "free option" attack is prevented by the Refund-reveals-secret design.

### Grief Resistance

Griefing attempts result in economic punishment for the griefer:

- Cardano User grief → loses ADA to Punish
- Monero User grief → wastes time, no profit

---

## Next Steps

Read the flow documents in order:

1. [01-happy-path.md](01-happy-path.md) - Understand the successful swap
2. [02-monero-user-griefs.md](02-monero-user-griefs.md) - Cancel/Refund mechanism
3. [03-cardano-user-never-locks.md](03-cardano-user-never-locks.md) - Lock order protection
4. [04-cardano-user-locks-abandons.md](04-cardano-user-locks-abandons.md) - Punish mechanism
5. [05-technical-failure.md](05-technical-failure.md) - Timeout safety
