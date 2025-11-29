# Protocol State Machines

This document provides state machine diagrams for both parties in the atomic swap protocol.

---

## Cardano User State Machine

```mermaid
stateDiagram-v2
    [*] --> Setup: Start swap

    Setup --> WaitingForMoneroLock: Lock ADA on Cardano
    Setup --> Aborted: Setup failed

    WaitingForMoneroLock --> Locked: Monero User locks XMR
    WaitingForMoneroLock --> CancelPublished: t₁ expired, Monero User didn't lock

    Locked --> SwapComplete: Monero User publishes Buy
    Locked --> CancelPublished: t₁ expired, no Buy received

    CancelPublished --> Refunded: Publish Refund
    CancelPublished --> Punished: t₂ expired without Refund

    SwapComplete --> [*]: +1 XMR, -100 ADA
    Refunded --> [*]: ±0 (recovered ADA)
    Punished --> [*]: -100 ADA (lost)
    Aborted --> [*]: ±0 (never locked)
```

**Monero User Grief Paths**:

- Never locks XMR: WaitingForMoneroLock → CancelPublished → Refunded
- Locks but doesn't Buy: Locked → CancelPublished → Refunded

### Cardano User State Descriptions

| State                    | Description                        | Actions Available                   |
| ------------------------ | ---------------------------------- | ----------------------------------- |
| **Setup**                | Exchanging keys and pre-signatures | Lock ADA, or abort                  |
| **WaitingForMoneroLock** | ADA locked, waiting for XMR        | Wait, or Cancel after t₁            |
| **Locked**               | Both parties have locked funds     | Wait for Buy, or Cancel after t₁    |
| **CancelPublished**      | Committed to abort path            | Refund (before t₂), or get Punished |
| **SwapComplete**         | Successfully swapped               | Extract y, claim XMR                |
| **Refunded**             | Recovered ADA via Refund           | Done                                |
| **Punished**             | Lost ADA to Punish                 | Done                                |
| **Aborted**              | Swap never started                 | Done                                |

---

## Monero User State Machine

```mermaid
stateDiagram-v2
    [*] --> Setup: Start swap

    Setup --> WaitingForCardanoLock: Generate y, exchange keys
    Setup --> Aborted: Setup failed

    WaitingForCardanoLock --> ReadyToLock: Cardano User locks ADA
    WaitingForCardanoLock --> Aborted: Timeout (Cardano User didn't lock)

    ReadyToLock --> Locked: Lock XMR in multisig

    Locked --> SwapComplete: Publish Buy
    Locked --> WaitingForRefund: Cardano User publishes Cancel

    WaitingForRefund --> Refunded: Cardano User publishes Refund
    WaitingForRefund --> Compensated: t₂ expired, publish Punish

    SwapComplete --> [*]: +100 ADA, -1 XMR
    Refunded --> [*]: ±0 (recovered XMR)
    Compensated --> [*]: +100 ADA, -1 XMR (locked)
    Aborted --> [*]: ±0 (never locked)
```

### Monero User State Descriptions

| State                     | Description                            | Actions Available                   |
| ------------------------- | -------------------------------------- | ----------------------------------- |
| **Setup**                 | Generating y, exchanging keys          | Continue setup, or abort            |
| **WaitingForCardanoLock** | Ready to lock, waiting for ADA         | Wait, or abort if timeout           |
| **ReadyToLock**           | Cardano lock verified                  | Lock XMR                            |
| **Locked**                | Both parties have locked funds         | Buy (claim ADA), or wait            |
| **WaitingForRefund**      | Cancel published, waiting for Refund   | Wait for Refund, or Punish after t₂ |
| **SwapComplete**          | Successfully swapped                   | Done                                |
| **Refunded**              | Recovered XMR via key extraction       | Done                                |
| **Compensated**           | Got ADA via Punish, XMR locked forever | Done                                |
| **Aborted**               | Swap never started                     | Done                                |

---

## Combined Protocol State Machine

```mermaid
stateDiagram-v2
    direction TB

    [*] --> Setup

    state Setup {
        [*] --> KeyExchange
        KeyExchange --> PreSignatures
        PreSignatures --> Ready
    }

    Setup --> LockPhase: Both parties ready

    state LockPhase {
        [*] --> CardanoLocking
        CardanoLocking --> CardanoLocked: Cardano User locks ADA
        CardanoLocked --> BothLocked: Monero User locks XMR
    }

    LockPhase --> Resolution: Both locked

    state Resolution {
        [*] --> WaitingForBuy

        WaitingForBuy --> HappyPath: Monero User publishes Buy
        WaitingForBuy --> AbortPath: t₁ expires, Cardano User publishes Cancel

        state HappyPath {
            [*] --> BuyPublished
            BuyPublished --> YExtracted: Cardano User extracts y
            YExtracted --> XMRClaimed: Cardano User claims XMR
        }

        state AbortPath {
            [*] --> CancelUTXO

            CancelUTXO --> RefundPath: Cardano User publishes Refund
            CancelUTXO --> PunishPath: t₂ expires, Monero User publishes Punish

            state RefundPath {
                [*] --> RefundPublished
                RefundPublished --> KeyExtracted: Monero User extracts s_cardano
                KeyExtracted --> XMRRecovered: Monero User recovers XMR
            }

            state PunishPath {
                [*] --> PunishPublished
                PunishPublished --> ADASeized: Monero User gets ADA
                note right of ADASeized: XMR locked forever
            }
        }
    }

    Resolution --> [*]
```

---

## Transaction Flow Diagram

```mermaid
flowchart TB
    subgraph Cardano["Cardano Chain"]
        Lock["Lock UTXO<br/>100 ADA"]
        Cancel["Cancel UTXO<br/>100 ADA"]
        Buy["Buy Tx<br/>reveals y"]
        Refund["Refund Tx<br/>reveals s_cardano"]
        Punish["Punish Tx"]

        Lock -->|"Buy (anytime)"| Buy
        Lock -->|"Cancel (after t₁)"| Cancel
        Cancel -->|"Refund (anytime)"| Refund
        Cancel -->|"Punish (after t₂)"| Punish
    end

    subgraph Monero["Monero Chain"]
        Multisig["2-of-2 Multisig<br/>1 XMR"]
        ClaimXMR["Claim XMR<br/>(using y)"]
        RecoverXMR["Recover XMR<br/>(using s_cardano)"]
        LockedForever["Locked Forever"]

        Multisig -->|"After Buy"| ClaimXMR
        Multisig -->|"After Refund"| RecoverXMR
        Multisig -.->|"After Punish"| LockedForever
    end

    Buy -->|"y extracted"| ClaimXMR
    Refund -->|"s_cardano extracted"| RecoverXMR
    Punish -.->|"s_cardano never revealed"| LockedForever

    style Buy fill:#90EE90
    style Refund fill:#90EE90
    style ClaimXMR fill:#90EE90
    style RecoverXMR fill:#90EE90
    style Punish fill:#FFB6C1
    style LockedForever fill:#FFB6C1
```

---

## State Transitions by Transaction

| Transaction        | From State      | To State             | Triggers                      | Reveals     |
| ------------------ | --------------- | -------------------- | ----------------------------- | ----------- |
| **Lock (Cardano)** | Setup           | WaitingForMoneroLock | Cardano User                  | —           |
| **Lock (Monero)**  | ReadyToLock     | Locked               | Monero User                   | —           |
| **Buy**            | Locked          | SwapComplete         | Monero User                   | `y`         |
| **Cancel**         | Locked          | CancelPublished      | Cardano User (after t₁)       | —           |
| **Refund**         | CancelPublished | Refunded             | Cardano User                  | `s_cardano` |
| **Punish**         | CancelPublished | Punished/Compensated | Monero User (after t₂)        | —           |
| **Claim XMR**      | SwapComplete    | Done                 | Cardano User (has `y`)        | —           |
| **Recover XMR**    | Refunded        | Done                 | Monero User (has `s_cardano`) | —           |

---

## Timeline with States

```
Time    Cardano User State          Monero User State           Possible Txs
─────────────────────────────────────────────────────────────────────────────
T=0     Setup                       Setup                       —
        │                           │
        ▼                           ▼
T+0     WaitingForMoneroLock        WaitingForCardanoLock       Lock (Cardano)
        │                           │
        │                           ▼
T+20m   WaitingForMoneroLock        ReadyToLock                 —
        │                           │
        ▼                           ▼
T+30m   Locked                      Locked                      Lock (Monero)
        │                           │
        │                           │
═══════════════════════════════════════════════════════════════════════════════
        HAPPY PATH
═══════════════════════════════════════════════════════════════════════════════
        │                           │
        │                           ▼
T+1h    Locked                      SwapComplete                Buy
        │                           │
        ▼                           │
T+2h    SwapComplete                SwapComplete                Claim XMR
        │                           │
        ▼                           ▼
        [+1 XMR, -100 ADA]          [+100 ADA, -1 XMR]

═══════════════════════════════════════════════════════════════════════════════
        ABORT PATH (if no Buy)
═══════════════════════════════════════════════════════════════════════════════
        │                           │
T+12h   t₁ expires                  t₁ expires                  —
        │                           │
        ▼                           ▼
T+12.5h CancelPublished             WaitingForRefund            Cancel
        │                           │
        ▼                           │
T+13h   Refunded                    WaitingForRefund            Refund
        │                           │
        │                           ▼
T+14h   Refunded                    Refunded                    Recover XMR
        │                           │
        ▼                           ▼
        [±0, recovered ADA]         [±0, recovered XMR]

═══════════════════════════════════════════════════════════════════════════════
        PUNISH PATH (if no Refund after Cancel)
═══════════════════════════════════════════════════════════════════════════════
        │                           │
T+12.5h CancelPublished             WaitingForRefund            Cancel
        │                           │
        │ (Cardano User offline)    │
        │                           │
T+36h   t₂ expires                  t₂ expires                  —
        │                           │
        ▼                           ▼
T+36.5h Punished                    Compensated                 Punish
        │                           │
        ▼                           ▼
        [-100 ADA, lost]            [+100 ADA, -1 XMR locked]
```

---

## Decision Points

### Cardano User Decisions

```mermaid
flowchart TD
    A[Locked State] --> B{Monero User<br/>published Buy?}
    B -->|Yes| C[Extract y, claim XMR]
    B -->|No| D{t₁ expired?}
    D -->|No| B
    D -->|Yes| E[Publish Cancel]
    E --> F[Publish Refund]
    F --> G[Recovered ADA]

    style C fill:#90EE90
    style G fill:#90EE90
```

### Monero User Decisions

```mermaid
flowchart TD
    A[Locked State] --> B{Want to<br/>complete swap?}
    B -->|Yes| C[Publish Buy]
    C --> D[Got ADA]
    B -->|No/Can't| E{Cardano User<br/>published Cancel?}
    E -->|No| B
    E -->|Yes| F{Cardano User<br/>published Refund?}
    F -->|Yes| G[Extract s_cardano]
    G --> H[Recover XMR]
    F -->|No| I{t₂ expired?}
    I -->|No| F
    I -->|Yes| J[Publish Punish]
    J --> K[Got ADA<br/>XMR locked forever]

    style D fill:#90EE90
    style H fill:#90EE90
    style K fill:#FFB6C1
```

---

## State Invariants

### Safety Properties

1. **No double-spend**: Each UTXO can only be spent once
   - Lock UTXO → Buy OR Cancel (not both)
   - Cancel UTXO → Refund OR Punish (not both)

2. **Atomicity**: If one party claims, the other can always claim
   - Buy reveals `y` → Cardano User can claim XMR
   - Refund reveals `s_cardano` → Monero User can recover XMR

3. **No free option**: Monero User cannot get both XMR and ADA
   - XMR recovery requires `s_cardano` from Refund tx
   - Punish means `s_cardano` never revealed → XMR locked forever

### Liveness Properties

1. **Bounded termination**: Protocol always terminates within t₁ + t₂ + confirmations
2. **Recovery possible**: Both parties can always recover funds (via abort path)
3. **No deadlock**: Every state has at least one valid transition
