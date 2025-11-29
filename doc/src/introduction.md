# Cardano-Monero Atomic Swap Protocol Documentation

> **Grief-Safe Atomic Swaps Using Adapter Signatures**

This documentation describes a production-ready atomic swap protocol between Cardano (scripted) and Monero (scriptless) blockchains, based on the COMIT/Farcaster design.

---

## Quick Start

### New to Atomic Swaps?

1. Read [00-shared-concepts.md](00-shared-concepts.md) for terminology and fundamentals
2. Read [01-happy-path.md](01-happy-path.md) to understand a successful swap
3. Skim the grief scenarios to understand protection mechanisms

### Implementing the Protocol?

1. Start with [00-shared-concepts.md](00-shared-concepts.md) for technical details
2. Implement happy path first ([01-happy-path.md](01-happy-path.md))
3. Add Cancel/Refund ([02-monero-user-griefs.md](02-monero-user-griefs.md))
4. Add Punish mechanism ([04-cardano-user-locks-abandons.md](04-cardano-user-locks-abandons.md))

### Security Auditing?

Read all documents in order, paying special attention to:

- **Refund reveals `s_cardano`** (NOT Cancel) — this is the key security property
- Punish timelock window (t₂ measured from Cancel)
- Race conditions between Refund and Punish after t₂

---

## Protocol Overview

### Parties

| Party            | Has | Wants | Key Role                                        |
| ---------------- | --- | ----- | ----------------------------------------------- |
| **Monero User**  | XMR | ADA   | Generates adapter secret `y`                    |
| **Cardano User** | ADA | XMR   | Locks first, holds Monero key share `s_cardano` |

### High-Level Flow

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         ATOMIC SWAP PROTOCOL                                │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  SETUP (Off-Chain)                                                          │
│  ═══════════════════                                                        │
│  1. Both parties generate Monero key shares (s_cardano, s_monero)           │
│  2. Monero User generates adapter secret y, shares Y = y·B                  │
│  3. Both parties exchange keys and create pre-signatures                    │
│                                                                             │
│  LOCK (On-Chain)                                                            │
│  ═══════════════════                                                        │
│  4. Cardano User locks ADA on Cardano (Plutus script)                       │
│  5. Monero User verifies, then locks XMR on Monero (2-of-2 multisig)        │
│                                                                             │
│  RESOLUTION (On-Chain)                                                      │
│  ═══════════════════                                                        │
│  6a. HAPPY: Monero User Buys ADA (reveals y) → Cardano User claims XMR      │
│  6b. ABORT: Cardano User Cancels → Refunds (reveals s_cardano) → Both refund│
│  6c. PUNISH: Cardano User Cancels but doesn't Refund → Gets punished        │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## Document Index

| Document                                                               | Description                               | When to Read     |
| ---------------------------------------------------------------------- | ----------------------------------------- | ---------------- |
| [00-shared-concepts.md](00-shared-concepts.md)                         | Terminology, cryptography, common phases  | First            |
| [01-happy-path.md](01-happy-path.md)                                   | Successful swap from start to finish      | Second           |
| [02-monero-user-griefs.md](02-monero-user-griefs.md)                   | Monero User doesn't Buy → Cancel + Refund | After happy path |
| [03-cardano-user-never-locks.md](03-cardano-user-never-locks.md)       | Lock order prevents this grief            | Reference        |
| [04-cardano-user-locks-abandons.md](04-cardano-user-locks-abandons.md) | Cancel without Refund → Punish            | Critical         |
| [05-technical-failure.md](05-technical-failure.md)                     | Network/software issues → Safe recovery   | Reference        |

---

## Scenario Comparison

| Scenario                          | Cardano User     | Monero User               | Mechanism       | Time      |
| --------------------------------- | ---------------- | ------------------------- | --------------- | --------- |
| **Happy Path**                    | -100 ADA, +1 XMR | +100 ADA, -1 XMR          | Buy tx          | ~2 hours  |
| **Monero User Griefs**            | ±0               | ±0                        | Cancel + Refund | ~14 hours |
| **Cardano User Never Locks**      | ±0               | ±0                        | Lock order      | Immediate |
| **Cardano User Cancels/Abandons** | **-100 ADA**     | **+100 ADA** (XMR locked) | Punish          | ~36 hours |
| **Technical Failure**             | ±0               | ±0                        | Cancel + Refund | ~14 hours |

### Key Observation

- **Monero User griefing**: No profit, both refund
- **Cardano User griefing**: Severe loss (entire swap amount), XMR locked forever

This asymmetry is intentional — the party who controls the adapter secret (Monero User) has less ability to grief, while the party who locks first (Cardano User) faces punishment for abandonment.

---

## Security Properties

### Atomicity

Either both parties receive their funds, or neither does.

```
Invariant: At no point can one party have claimed
           while the other cannot claim.
```

### No "Free Option" Attack

**Critical design property**: Monero User cannot get both XMR and ADA.

```
- If Cardano User Refunds → s_cardano revealed → Monero User recovers XMR
                         → ADA returned to Cardano User
                         → No Punish possible

- If Cardano User doesn't Refund → Monero User Punishes → Gets ADA
                                 → s_cardano never revealed
                                 → XMR locked forever
```

### Liveness

The protocol always terminates within bounded time.

```
Maximum time to resolution: Cancel + t₂ + confirmation time (~36 hours)
```

### Fairness

Neither party can gain advantage through deviation.

```
Optimal strategy for both parties: Follow protocol honestly
```

### Grief Resistance

Griefing attempts are economically irrational.

```
Cost of griefing:
- Monero User: Time wasted (no funds lost)
- Cardano User: Entire swap amount (punished)
```

---

## Timelock Summary

| Timelock | Opens                  | Purpose                                         |
| -------- | ---------------------- | ----------------------------------------------- |
| t₁       | ~12 hours after Lock   | Cancel window (Cardano User can initiate abort) |
| t₂       | ~24 hours after Cancel | Punish window (Monero User can punish)          |

```
Timeline:
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

Before t₁: Only Buy available (from Lock UTXO)
After t₁:  Cancel available (moves ADA to Cancel UTXO)
After Cancel: Refund available (reveals s_cardano)
After t₂:  Punish available (from Cancel UTXO)
```

**Critical**: t₂ is measured from Cancel, NOT from Lock.

---

## Transaction Summary

### On Cardano (Scripted/Arbitrating Chain)

| Transaction | Who          | When          | Reveals     | Effect                                |
| ----------- | ------------ | ------------- | ----------- | ------------------------------------- |
| **Lock**    | Cardano User | Setup         | —           | Locks ADA in Plutus script            |
| **Buy**     | Monero User  | Before Cancel | `y`         | Transfers ADA to Monero User          |
| **Cancel**  | Cardano User | After t₁      | **Nothing** | Commits to abort, creates Cancel UTXO |
| **Refund**  | Cardano User | After Cancel  | `s_cardano` | Returns ADA to Cardano User           |
| **Punish**  | Monero User  | After t₂      | —           | Takes all ADA as penalty              |

### On Monero (Scriptless/Accordant Chain)

| Transaction | Who          | When                           | Effect                        |
| ----------- | ------------ | ------------------------------ | ----------------------------- |
| **Lock**    | Monero User  | After Cardano lock             | Creates 2-of-2 multisig       |
| **Claim**   | Cardano User | After Buy (has `y`)            | Spends multisig, gets XMR     |
| **Refund**  | Monero User  | After Refund (has `s_cardano`) | Spends multisig, recovers XMR |

---

## Design Decisions

### Why Monero User Generates Adapter Secret?

The party on the scriptless chain generates the adapter secret because:

1. They claim first (on the scripted chain via Buy)
2. Their claim reveals the secret `y`
3. This enables the other party to claim on the scriptless chain

### Why Cardano User Locks First?

Lock order (scripted first) because:

1. Scripted chain has Cancel mechanism for recovery
2. Scriptless chain has no timelocks (no recovery if counterparty disappears)
3. Party locking second can verify first lock before committing

### Why Two Transactions (Cancel + Refund)?

Cancel and Refund are separate because:

1. **Cancel**: Commits to abort path, eliminates Buy race, protects from Punish race
2. **Refund**: Reveals `s_cardano`, enables Monero recovery

If combined, Cardano User would face a three-way race (Buy vs Refund vs Punish) after t₂.

### Why Refund Reveals Secret (Not Cancel)?

**This is the critical security property.**

If Cancel revealed `s_cardano`:

- Monero User would recover XMR immediately
- Monero User could ALSO race for Punish
- "Free option" attack: potentially get both XMR AND ADA

With Refund revealing `s_cardano`:

- Monero User cannot recover XMR until Cardano User Refunds
- If Cardano User Refunds → both parties recover (fair)
- If Cardano User doesn't Refund → Punish, but XMR locked forever

---

## References

- **COMIT XMR-BTC RFC**: Original protocol design for Bitcoin-Monero swaps
- **Farcaster RFC-08**: Transaction structure specification
- **Zhu et al. 2024**: "Adaptor signature based on randomized EdDSA in blockchain"

---

## Quick Reference: What Happens If...

| If...                                 | Then...                         | See                                                                    |
| ------------------------------------- | ------------------------------- | ---------------------------------------------------------------------- |
| Everything works                      | Both swap successfully          | [01-happy-path.md](01-happy-path.md)                                   |
| Monero User doesn't Buy               | Both refund via Cancel + Refund | [02-monero-user-griefs.md](02-monero-user-griefs.md)                   |
| Cardano User doesn't lock             | Nothing happens                 | [03-cardano-user-never-locks.md](03-cardano-user-never-locks.md)       |
| Cardano User Cancels + doesn't Refund | Gets punished, XMR locked       | [04-cardano-user-locks-abandons.md](04-cardano-user-locks-abandons.md) |
| Network/software fails                | Same as grief: Cancel + Refund  | [05-technical-failure.md](05-technical-failure.md)                     |
