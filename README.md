# Atomic Swap Tutorial with Adapter Signatures

An educational Haskell implementation demonstrating atomic swaps using **rEdDSA adapter signatures** on Ed25519.

> **Status**: See [STATUS.md](STATUS.md) for current implementation progress and test results.

## What is This?

This project teaches atomic swap protocols through executable code. Watch Alice and Bob exchange digital assets across two blockchains without trusted intermediaries, using adapter signatures—a cryptographic primitive that enables "scriptless scripts."

### Key Features

- **End-to-end protocol**: Complete atomic swap from setup to execution
- **Verbose logging**: Observe every protocol step
- **Multi-threaded**: Alice and Bob run as separate threads with STM-based messaging
- **Comprehensive tests**: Unit tests for crypto primitives, integration tests for protocol
- **Educational focus**: Well-documented code with step-by-step explanations

## Quick Start

### Prerequisites

- Nix with flakes enabled
- Basic Haskell knowledge

### Running Tests

```bash
# Enter development environment
nix develop

# Run all tests
cabal test

# Run specific scenario
cabal test --test-options="-m HappyPath"
```

### Build Commands

See [CLAUDE.md](CLAUDE.md#build-and-test-commands) for complete build and development commands.

## Documentation

All documentation is organized by purpose:

| Document                                             | Purpose                                                        |
| ---------------------------------------------------- | -------------------------------------------------------------- |
| **[STATUS.md](STATUS.md)**                           | Current implementation status, module completion, test results |
| **[Protocol Documentation](doc/)**                   | Cardano-Monero swap protocol specification with scenarios      |
| **[TUTORIAL.md](TUTORIAL.md)**                       | Step-by-step code walkthrough with explanations                |
| **[IMPLEMENTATION-PLAN.md](IMPLEMENTATION-PLAN.md)** | Week-by-week implementation roadmap and design decisions       |
| **[CLAUDE.md](CLAUDE.md)**                           | Developer guide for working with this codebase                 |

### Research Materials

Detailed research documents are in the `research/` directory:

- `research/2025-11-14-ed25519-adapter-signatures.md` - Complete rEdDSA specification (44KB)
- `research/2025-11-14-two-party-atomic-swap-protocol.md` - Protocol design analysis
- `pdf/1-s2.0-S2352864824000713-main.pdf` - Zhu et al. (2024) paper

## Learning Path

1. **Overview**: Read this README
2. **Protocol**: Read [Protocol Documentation](doc/) to understand the cryptographic protocol
3. **Status**: Check [STATUS.md](STATUS.md) to see what's implemented
4. **Code**: Follow [TUTORIAL.md](TUTORIAL.md) for code walkthrough
5. **Deep dive**: Read research documents for cryptographic details

## What You'll Learn

### Cryptographic Concepts

- Adapter signatures (verifiably encrypted signatures)
- Randomized EdDSA (rEdDSA) on Ed25519
- Scriptless scripts
- Secret extraction from signature pairs
- Zero-knowledge proofs (Schnorr NIZK)

### Protocol Design

- Atomic swap protocol phases
- Security properties (atomicity, fairness, privacy)
- Thread-based protocol execution
- STM-based message passing

### Blockchain Concepts

- UTXO transaction model
- Signature-based authorization
- Cross-chain interoperability

### Haskell Patterns

- io-classes for polymorphic concurrency
- io-sim for deterministic testing
- STM for message passing
- Functional core / imperative shell

## Project Structure

See [CLAUDE.md](CLAUDE.md#module-organization) for detailed module organization.

```
src/AtomicSwap/
├── Types.hs              # Core types
├── Crypto/               # rEdDSA implementation
├── Blockchain/           # UTXO simulation
├── Protocol/             # Alice and Bob threads
└── Logging.hs            # Protocol logging

test/AtomicSwap/
├── CryptoSpec.hs         # Crypto primitives tests
├── BlockchainSpec.hs     # Blockchain tests
└── [Integration tests]   # Protocol scenarios
```

## Development

All development commands and code quality standards are documented in [CLAUDE.md](CLAUDE.md).

### Quick Reference

```bash
# Build with zero-warning enforcement
cabal build --ghc-options=-Werror

# Format code
treefmt

# Run linter
hlint src/ test/

# Pre-commit hooks (auto-formats and lints)
pre-commit run --all-files
```

## References

### Academic Papers

- **Zhu et al. (2024)**: "Adaptor signature based on randomized EdDSA in blockchain"
- **Poelstra (2017)**: "Scriptless Scripts"
- **Fournier (2019)**: "Adaptor Signatures"

### Implementations

- **Farcaster**: Bitcoin-Monero atomic swaps
- **AthanorLabs**: Ethereum-Monero atomic swaps
- **Lightning Network**: Payment channel adapters

## License

Apache-2.0

## Acknowledgments

- Research informed by Farcaster and cryptography literature
- Educational approach inspired by "learn by doing" philosophy

---

**For current implementation status**, see [STATUS.md](STATUS.md).
**For development guidance**, see [CLAUDE.md](CLAUDE.md).
