# TLA+ Formal Verification Plan

## Overview

Add comprehensive TLA+ formal specification to verify the atomic swap protocol's safety and liveness properties. The specification will model both chains explicitly and verify all documented scenarios.

## Directory Structure

```
.local/formal/
├── AtomicSwap.tla           # Core state machine and transitions
├── AtomicSwapMC.tla         # Model checking module (instantiates constants)
├── AtomicSwap.cfg           # TLC model checker configuration
└── README.md                # How to run verification
```

## Implementation Steps

### 1. Add TLA+ tooling to flake.nix

Add to `devShells.default.buildInputs`:

```nix
tlaplus  # Provides tlc2, sany, tla2tools
```

### 2. Create core TLA+ specification (`AtomicSwap.tla`)

#### Variables

```tla
VARIABLES
  \* Party states
  cardanoUserState,    \* Setup | WaitingForMoneroLock | Locked | CancelPublished | SwapComplete | Refunded | Punished | Aborted
  moneroUserState,     \* Setup | WaitingForCardanoLock | ReadyToLock | Locked | WaitingForRefund | SwapComplete | Refunded | Compensated | Aborted

  \* Blockchain state
  lockUTXO,            \* none | unspent | spent_buy | spent_cancel
  cancelUTXO,          \* none | unspent | spent_refund | spent_punish
  moneroMultisig,      \* none | locked | spent_by_cardano | spent_by_monero | locked_forever

  \* Secrets
  yRevealed,           \* TRUE if adapter secret y is public (via Buy tx)
  sCardanoRevealed,    \* TRUE if s_cardano is public (via Refund tx)

  \* Time
  time                 \* Current time slot
```

#### Constants

```tla
CONSTANTS
  T1,                  \* Cancel timelock (e.g., 12 hours)
  T2                   \* Punish timelock from Cancel (e.g., 24 hours)
```

#### Actions

| Action        | Guard                                           | Effect                               |
| ------------- | ----------------------------------------------- | ------------------------------------ |
| `CardanoLock` | cardanoUserState = Setup                        | Create lockUTXO, advance state       |
| `MoneroLock`  | moneroUserState = ReadyToLock                   | Create moneroMultisig, advance state |
| `Buy`         | lockUTXO = unspent                              | Spend lockUTXO, reveal y             |
| `Cancel`      | lockUTXO = unspent /\ time >= T1                | Spend lockUTXO, create cancelUTXO    |
| `Refund`      | cancelUTXO = unspent                            | Spend cancelUTXO, reveal s_cardano   |
| `Punish`      | cancelUTXO = unspent /\ time >= cancelTime + T2 | Spend cancelUTXO                     |
| `ClaimXMR`    | yRevealed /\ moneroMultisig = locked            | Cardano User spends multisig         |
| `RecoverXMR`  | sCardanoRevealed /\ moneroMultisig = locked     | Monero User spends multisig          |
| `Tick`        | TRUE                                            | Advance time                         |

### 3. Safety Properties

```tla
\* CRITICAL: Monero User cannot get both XMR and ADA
NoFreeOption ==
  ~(moneroUserState = Compensated /\ moneroMultisig = spent_by_monero)

\* Atomicity: Buy reveals y, enabling Cardano User to claim XMR
Atomicity ==
  (lockUTXO = spent_buy) => yRevealed

\* Atomicity: Refund reveals s_cardano, enabling Monero User to recover XMR
RefundEnablesRecovery ==
  (cancelUTXO = spent_refund) => sCardanoRevealed

\* Cancel reveals NOTHING (critical security property)
CancelRevealsNothing ==
  (cancelUTXO # none) => ~sCardanoRevealed \/ (cancelUTXO = spent_refund)

\* No double-spend on Lock UTXO
LockUTXOExclusive ==
  ~(lockUTXO = spent_buy /\ lockUTXO = spent_cancel)

\* No double-spend on Cancel UTXO
CancelUTXOExclusive ==
  ~(cancelUTXO = spent_refund /\ cancelUTXO = spent_punish)

\* Monero multisig spent at most once
MoneroMultisigExclusive ==
  ~(moneroMultisig = spent_by_cardano /\ moneroMultisig = spent_by_monero)
```

### 4. Liveness Properties

```tla
\* Protocol always terminates
Termination ==
  <>(cardanoUserState \in {SwapComplete, Refunded, Punished, Aborted})

\* If Cardano User locks, they eventually reach terminal state
CardanoRecovery ==
  (cardanoUserState = WaitingForMoneroLock) ~>
    (cardanoUserState \in {SwapComplete, Refunded, Punished})

\* If both lock, at least one party claims
NoStalemate ==
  (cardanoUserState = Locked /\ moneroUserState = Locked) ~>
    (lockUTXO # unspent)

\* Fairness: If Monero User can Buy, they eventually do or time advances
MoneroFairness ==
  WF_vars(Buy) \/ WF_vars(Tick)
```

### 5. Scenarios to Verify

| Scenario               | Initial State       | Expected Terminal States |
| ---------------------- | ------------------- | ------------------------ |
| Happy Path             | Both setup complete | SwapComplete (both)      |
| Monero never locks     | Cardano locks       | Refunded (Cardano)       |
| Monero locks, no Buy   | Both lock           | Refunded (both)          |
| Cardano doesn't Refund | Cancel published    | Punished / Compensated   |
| Setup fails            | Setup               | Aborted (both)           |

### 6. Model Checker Configuration (`AtomicSwap.cfg`)

```
SPECIFICATION Spec

\* Safety (checked as invariants)
INVARIANT NoFreeOption
INVARIANT Atomicity
INVARIANT RefundEnablesRecovery
INVARIANT CancelRevealsNothing
INVARIANT LockUTXOExclusive
INVARIANT CancelUTXOExclusive
INVARIANT MoneroMultisigExclusive
INVARIANT TypeInvariant

\* Liveness (checked as temporal properties)
PROPERTY Termination
PROPERTY CardanoRecovery
PROPERTY NoStalemate

\* Constants
CONSTANT T1 = 3
CONSTANT T2 = 5
```

### 7. README Documentation

````markdown
# Formal Verification

## Prerequisites

Enter nix development shell:

```bash
nix develop
```

## Run Verification

Check syntax:

```bash
java -cp $TLA2TOOLS tla2sany.SANY AtomicSwap.tla
```

Run model checker:

```bash
java -cp $TLA2TOOLS tlc2.TLC -config AtomicSwap.cfg AtomicSwapMC.tla
```

## Expected Results

All invariants should hold. TLC will report:

- States explored
- Distinct states
- Any counterexamples (should be none)
````

## Files to Create/Modify

| File                             | Action | Description                        |
| -------------------------------- | ------ | ---------------------------------- |
| `flake.nix`                      | Modify | Add `tlaplus` to buildInputs       |
| `.local/formal/AtomicSwap.tla`   | Create | Core specification (~200 lines)    |
| `.local/formal/AtomicSwapMC.tla` | Create | Model checking wrapper (~30 lines) |
| `.local/formal/AtomicSwap.cfg`   | Create | TLC configuration (~20 lines)      |
| `.local/formal/README.md`        | Create | Usage documentation                |

## Key Properties Summary

| Property              | Type     | Ensures                                   |
| --------------------- | -------- | ----------------------------------------- |
| NoFreeOption          | Safety   | Monero User cannot get both XMR and ADA   |
| Atomicity             | Safety   | Buy reveals y for XMR claim               |
| RefundEnablesRecovery | Safety   | Refund reveals s_cardano for XMR recovery |
| CancelRevealsNothing  | Safety   | Cancel alone doesn't enable XMR recovery  |
| Termination           | Liveness | Protocol always completes                 |
| CardanoRecovery       | Liveness | Cardano User can always recover           |
| NoStalemate           | Liveness | Locked state always resolves              |

## Estimated Effort

- Tooling setup (flake.nix): 10 minutes
- Core specification: 2-3 hours
- Property definitions: 1 hour
- Testing and debugging: 2-3 hours
- Documentation: 30 minutes

**Total: ~1 day**

---

## Deferred Enhancements

The following enhancements were identified during protocol audit but deferred for future implementation:

### Race Condition Modeling

Extend specification to model the t₂ race condition between Refund and Punish:

- Add `MempoolState` variable to track pending transactions
- Model fee-competitive scenarios (dynamic vs pre-signed fees)
- Add `RaceResolution` action for block producer selection

### Network Partition Scenarios

Add Byzantine fault tolerance analysis:

- Model partial network connectivity
- Verify safety under message delays
- Add `NetworkPartition` operator

### Cryptographic Soundness

The TLA+ spec assumes cryptographic primitives are correct. Future work:

- Reference cryptographic proofs for rEdDSA adapter signatures
- Link to Zhu et al. (2024) security analysis
- Model cryptographic failure modes (key compromise)

### Economic Incentive Analysis

Formal game-theoretic analysis of:

- Implicit option value quantification
- Griefing cost/benefit ratios
- Fee market dynamics impact on race conditions
