# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Meta: Self-Maintenance Protocol

**CRITICAL**: Update documentation in the SAME commit as code changes.

**Mandatory Workflow**:

1. Make code changes (add module, implement feature, etc.)
2. **IMMEDIATELY** update relevant documentation
3. **Commit BOTH together** in single commit
4. Never defer documentation updates to separate commits

**Auto-update triggers** → **Update these files**:

- Adding/removing modules → `CLAUDE.md` (Module Organization) + `STATUS.md` (Module Status table)
- Adding dependencies → `CLAUDE.md` (Key libraries in Cryptographic Details)
- Changing build/test commands → `CLAUDE.md` (Build and Test Commands) only
- New patterns discovered → `CLAUDE.md` (Common Pitfalls)
- Major milestone completed → `STATUS.md` (Current Status Summary)
- Test results changed → `STATUS.md` (Test Coverage section)
- Design decisions made → `IMPLEMENTATION-PLAN.md` (document rationale)

**Documentation Single Source of Truth**:

- `STATUS.md` → Implementation progress, module completion, test results
- `CLAUDE.md` → Code patterns, build commands, module organization
- `IMPLEMENTATION-PLAN.md` → Design decisions, roadmap
- `README.md` → References other docs (rarely updated)

**What NOT to update**:

- Don't add generic advice that applies to all projects
- Don't duplicate information between docs (use references)
- Don't add every minor detail discoverable via Read/Grep
- Don't include temporary/exploratory code patterns

## Project Context

Educational Haskell tutorial demonstrating atomic swaps using **rEdDSA adapter signatures** on Ed25519. Prioritizes clarity, comprehensive documentation, and test coverage.

**Target Audience**: Technical readers who can read Haskell
**Primary Goal**: Teach end-to-end atomic swap protocol with working code
**Cryptography**: rEdDSA adapter signatures (Zhu et al. 2024)

**Quick Documentation Reference**:

- Current status? → See [STATUS.md](STATUS.md)
- Design decisions? → See [IMPLEMENTATION-PLAN.md](IMPLEMENTATION-PLAN.md)
- Protocol details? → See [PROTOCOL.md](PROTOCOL.md)
- Code walkthrough? → See [TUTORIAL.md](TUTORIAL.md)
- User overview? → See [README.md](README.md)

## Build and Test Commands

```bash
# Enter development environment
nix develop

# Build (enforces zero warnings)
cabal build --ghc-options=-Werror

# Run all tests
cabal test

# Run specific test scenario
cabal test --test-options="-m HappyPath"

# Format all code (single source of truth)
treefmt

# Run all pre-commit hooks (treefmt + hlint)
pre-commit run --all-files

# Lint only
hlint src/ test/
```

**Note**: Test output format (`--test-show-details=streaming`) is configured in `cabal.project`.

## Development Workflow (Simulator)

### Auto-rebuild with watchman-make (Recommended for Claude Code)

Run simulator with automatic rebuild/restart on file changes:

```bash
# Start watchman-make in background (Claude Code can use this)
watchman-make \
  -p 'src/**/*.hs' 'app/**/*.hs' '*.cabal' \
  --run 'pkill -9 -x atomic-swap-simulator; cabal build --ghc-options=-Werror && nohup cabal run atomic-swap-simulator > /tmp/simulator.log 2>&1 &' &

# Check server logs
tail -f /tmp/simulator.log

# Stop watchman and server
pkill -9 -f watchman; pkill -9 -x atomic-swap-simulator
```

**Behavior**:

- Watches `src/**/*.hs`, `app/**/*.hs`, `*.cabal`
- On file change: kills old server, rebuilds, restarts server
- Server runs on http://localhost:8888
- Logs to `/tmp/simulator.log`
- If build fails, server is NOT restarted (old version keeps running)

**For Claude Code**: Use the watchman-make command as a background process. It will automatically restart the server after every code change.

## Code Architecture

### Module Organization

```
src/AtomicSwap/
├── Types.hs              # Core types with hex-encoded Show instances
├── Types/
│   └── Orphans.hs        # JSON instances (FromJSON/ToJSON) - isolated orphans
├── Prelude.hs            # Custom project prelude (re-exports Relude + core types)
├── Crypto/               # Cryptographic primitives (rEdDSA)
│   ├── Keys.hs           # Ed25519 keypair generation
│   ├── Signatures.hs     # rEdDSA signing/verification
│   ├── Adapter.hs        # Adapter signature construction
│   └── NIZK.hs           # Schnorr discrete log proofs
├── Blockchain/           # Simulated blockchain (JSON ledger)
│   ├── Types.hs          # Blockchain-specific types
│   ├── Ledger.hs         # File-based ledger operations
│   └── Transaction.hs    # UTXO transaction logic
├── Protocol/             # Atomic swap protocol
│   ├── Alice.hs          # Alice's protocol thread (TODO)
│   ├── Bob.hs            # Bob's protocol thread (TODO)
│   └── Messaging.hs      # STM-based message passing (TODO)
└── Logging.hs            # Verbose protocol logging (TODO)

test/AtomicSwap/
├── BlockchainSpec.hs     # Blockchain tests (10 tests) ✅
├── CryptoSpec.hs         # Crypto primitives tests (17 tests) ✅
├── HappyPathSpec.hs      # Successful swap scenario (TODO)
├── RefundSpec.hs         # Timeout refund scenario (TODO)
└── ErrorHandlingSpec.hs  # Error cases (TODO)
```

### Key Design Patterns

**Interactive Simulator Architecture (State-Diffing + Event Sourcing)**:

- **GlobalState**: Append-only log of StateUpdates (single source of truth)
- **PartyState**: Read-only projections computed by folding GlobalState
- **appendStep**: Only mutation point (appends to log, reconstructs all projections)
- **Step Handlers**: Pure event appenders (`m ()` or `m Bool`), don't know about rendering
- **HTTP Layer**: Diffs state before/after, detects changed parties, renders only affected UI panels
- **Benefits**: Automatic cross-participant updates, simpler handlers, bug elimination

Pattern for step execution:

```haskell
-- Handler (pure event appender)
executeStep :: MonadSimulator m => m Bool
executeStep = do
  state <- getPartyState Alice  -- Read projection
  applyUpdates Alice inputs [update1, update2]  -- Append to log
  pure True  -- No rendering concern

-- HTTP layer (state-diffing)
handleStep = do
  oldState <- readIORef stateRef
  success <- runSimulatorT stateRef executeStep
  newState <- readIORef stateRef
  changedParties <- detectChangedParties oldState newState  -- Diff
  respond $ renderStateUpdates changedParties newState  -- Render only changed
```

See `src/AtomicSwap/Simulator/State.hs:10-19` for detailed architecture documentation.

**Cryptography (Zhu et al. 2024 rEdDSA)**:

- Split private key: `(sk0, sk1)` where `sk0` = signing scalar, `sk1` = nonce seed
- Randomized nonce: `r = H2(sk1 || msg || k)` with fresh random `k`
- Adapter pre-signature: `sig_tilde = r + h·sk0` (WITHOUT adapter secret)
- Adapted nonce: `R_sign = r·B + Y` (ADD adapter point)
- Complete: `sig = sig_tilde + y` (Alice), Extract: `y = sig - sig_tilde` (Bob)

**Thread Communication & Testability (io-classes Pattern)**:

- Alice and Bob run as separate threads using io-classes/io-sim
- STM-based TQueue for message passing (polymorphic over `MonadSTM m`)
- File-based blockchain ledgers with TVar state (polymorphic over `MonadSTM m`)
- **Functional Core**: Protocol logic polymorphic: `(MonadSTM m, MonadRandom m, MonadIO m)`
- **Imperative Shell**: Concrete IO execution for production
- **Pure Testing**: io-sim for deterministic execution without IO
- **Key abstraction**: `Blockchain m` where `m` can be `IO` or `IOSim s`

**Hex-Encoded Types**:

- All ByteString newtypes derive Show via HexBytes wrapper
- Pattern: `newtype Foo = Foo ByteString deriving (Show) via HexBytes`
- Defined in `AtomicSwap.Types` with `DerivingVia`

**Custom Prelude (io-classes Integration)**:

- `AtomicSwap.Prelude` exports all core types + io-classes STM primitives
- **IMPORTANT**: Hides Relude's STM (done in cabal mixins for global effect)
- Re-exports io-classes: `MonadSTM`, `TVar`, `TQueue`, `atomically`, etc.
- Provides project utilities: `logInfo`, `logPhase`, `EitherText`
- Modules importing Prelude get io-sim compatible concurrency primitives
- Pattern: Modules using STM must `import Prelude hiding (STM, TVar, atomically, ...)`

**Orphan Instance Organization**:

- All orphan instances isolated in `AtomicSwap.Types.Orphans`
- JSON instances for PublicKey, TxId, Signature, UTXO, Output, Transaction
- Marked with `{-# OPTIONS_GHC -Wno-orphans #-}`
- Import where JSON needed: `import AtomicSwap.Types.Orphans ()`

## Haskell Style Standards

### Code Formatting

**Single Source of Truth**: All formatting is handled by `treefmt` configured in `treefmt.toml`

- **Formatter**: treefmt orchestrates fourmolu, cabal-fmt, nixfmt, prettier
- **Pre-commit hooks**: Run treefmt automatically (no individual formatter hooks)
- **Manual formatting**: `treefmt` (never run individual formatters)
- **Configuration**: All fourmolu options specified as CLI args in treefmt.toml

**Fourmolu Settings** (enforced via treefmt):

- **Indentation**: 2 spaces (never tabs)
- **Line length**: Maximum 80 characters
- **Comma style**: Leading (`, field` not `field,`)
- **Single constraint parens**: Never (`MonadSTM m` not `(MonadSTM m)`)
- **Single deriving parens**: Never (`deriving stock Eq` not `deriving stock (Eq)`)
- **Function arrows**: Leading
- **Import/export style**: Leading
- **Unicode**: Never (ASCII only by default)
- **Prelude**: Relude with explicit unqualified imports
- **Section comments**: 80-char divider lines for organization

### Modern Haskell Extensions

- **BlockArguments**: Required (`it "test" do` not `it "test" $ do`)
  - **Style preference**: Never use `$` before lambda as last argument
  - Use `foo \x -> ...` instead of `foo $ \x -> ...`
  - Use `bar \case` instead of `bar $ \case`
- **DerivingVia**: For consistent Show instances (see HexBytes pattern)
- **ImportQualifiedPost**: Required (`import Data.Map qualified as Map`)

### Type Safety Principles

**Type-Indexed Newtypes Over Primitives:**

- Prefer: `Quantity 'Apple` over `Natural` over `Int`
- Pattern: `newtype Quantity (asset :: Asset) = Quantity Natural`
- Prevents mixing incompatible values (apples vs bananas)
- Provides type-level guarantees and better compiler errors

**When designing types:**

1. Start with most specific type possible
2. Use GADTs/DataKinds for type-level constraints
3. Avoid primitive types (Int, Text, ByteString) for domain values
4. Derive common instances (Eq, Ord, Show, NFData, NoThunks)

### CSS and Styling

**CSS-First Principle**: Always define styles in `static/style.css`, never use inline styles in Lucid HTML.

- Add CSS classes to stylesheet
- Reference classes in HTML: `div_ [class_ "my-class"]`
- Only use inline styles if absolutely necessary (e.g., dynamic computed values)
- Rationale: Maintainability, consistency, separation of concerns

### Zero-Warning Standard

**MANDATORY**: All code must compile with zero warnings.

```bash
# Always verify after changes
cabal build --ghc-options=-Werror
```

**Workflow**:

1. Write/modify code
2. Build with `-Werror`
3. Fix ALL warnings immediately
4. Only proceed after clean build

**Common warnings**:

- Unused imports → Remove (Relude exports many things)
- Name shadowing → Rename variables
- Missing type signatures → Add explicit types
- Incomplete patterns → Handle all cases

### Test-Driven Development

**MANDATORY**: Write comprehensive tests immediately after implementing each component.

**Pattern**:

1. Implement module
2. **IMMEDIATELY** write tests (before next module)
3. Verify test coverage: `cabal test --test-show-details=streaming`
4. Fix all failures
5. Only then proceed

**Test requirements**:

- Use BlockArguments syntax
- Test happy paths AND edge cases
- HSpec framework with descriptive names
- Scenario-based integration tests

## Documentation Architecture

**Single Source of Truth Principle**: Each piece of information lives in exactly one file. Other files reference it.

### Documentation Files

| File                     | Purpose                                                    | Audience                 |
| ------------------------ | ---------------------------------------------------------- | ------------------------ |
| `README.md`              | Project overview with references to other docs             | End users, learners      |
| `STATUS.md`              | **Implementation status**, module completion, test results | Developers, contributors |
| `PROTOCOL.md`            | 35-step protocol specification with security analysis      | Technical readers        |
| `TUTORIAL.md`            | Step-by-step code walkthrough                              | Learners                 |
| `IMPLEMENTATION-PLAN.md` | Week-by-week roadmap, **design decisions**                 | Developers               |
| `CLAUDE.md`              | Developer guide, **code patterns**, build commands         | Claude Code              |
| `research/*.md`          | Cryptographic specifications and research                  | Technical deep-dive      |

### Update Rules

**When completing milestones**:

- Update `STATUS.md` → Module completion, test results, build status
- Update `IMPLEMENTATION-PLAN.md` → Check off completed tasks
- Do NOT update README.md (it references STATUS.md)

**When adding/removing modules**:

- Update `CLAUDE.md` → Module Organization tree
- Update `STATUS.md` → Module Status table

**When changing build/test commands**:

- Update `CLAUDE.md` → Build and Test Commands section only

**When documenting patterns**:

- Update `CLAUDE.md` → Key Design Patterns section

## Cryptographic Implementation Details

**Reference**: Zhu et al. (2024) "Adaptor signature based on randomized EdDSA in blockchain"

- Paper: `pdf/1-s2.0-S2352864824000713-main.pdf`
- Specification: `research/2025-11-14-ed25519-adapter-signatures.md`

**Key libraries**:

- `cryptonite` - Ed25519 curve operations (`Crypto.ECC.Edwards25519`)
- `Crypto.Hash` - SHA-512 for H1/H2 hash functions
- `Crypto.Number.Serialize.LE` - Little-endian serialization

**Critical constants**:

- Ed25519 group order: `2^252 + 27742317777372353535851937790883648493`
- Key size: 32 bytes
- Signature size: 64 bytes

**NIZK Proofs**: Schnorr sigma protocol with Fiat-Shamir transform

## Protocol Flow (Simplified V1)

Alice and Bob exchange assets atomically across two blockchains:

1. **Setup**: Alice generates adapter secret `y`, commitment `Y = y·B`, sends Y to Bob
2. **Pre-signatures**: Both create adapted pre-signatures using same Y
3. **Atomic execution**: Alice publishes first (reveals `y`), Bob extracts `y` and completes

**Key property**: Alice cannot claim Bob's funds without revealing `y`, and once revealed, Bob can always extract it.

## Complex Project Patterns

This project demonstrates a research-heavy initialization pattern:

1. **Requirements discovery**: `/interview` to capture ambiguities
2. **Technical research**: Deep dive into papers, algorithms, specifications
3. **Decision documentation**: `IMPLEMENTATION-PLAN.md` with user-confirmed choices
4. **Systematic implementation**: Follow plan with test coverage

**When to use**:

- Unfamiliar technologies (novel cryptography, protocols)
- Multiple viable approaches
- Educational/tutorial projects requiring comprehensive docs

## Testing Strategy

**Current coverage**: 27/27 tests passing (22 implemented, 5 pending)

**Test structure**:

- `CryptoSpec.hs` - Unit tests for cryptographic primitives (17 tests) ✅
- `BlockchainSpec.hs` - Blockchain simulation tests (10 tests) ✅
- `HappyPathSpec.hs` - Complete atomic swap scenario (pending)
- `RefundSpec.hs` - Timeout/refund paths (pending)
- `ErrorHandlingSpec.hs` - Invalid signatures, insufficient funds (pending)

**Test isolation**: Each scenario uses temporary blockchain files, independent message queues, cleanup after completion.

## Development Workflow

**For new features**:

1. Update `IMPLEMENTATION-PLAN.md` with design
2. Implement module following rEdDSA spec
3. Write comprehensive tests immediately
4. Verify `cabal build --ghc-options=-Werror`
5. **Update CLAUDE.md** if module structure/patterns changed
6. Update `STATUS.md` progress
7. Pre-commit hooks run `treefmt` automatically (no manual formatting needed)
8. **Commit code + CLAUDE.md + STATUS.md together**

**For debugging**:

- Use verbose logging (see `AtomicSwap.Logging`)
- Check test output with `--test-show-details=streaming`
- Verify cryptographic operations match Zhu et al. specification

## Common Pitfalls

- **Endianness**: Use little-endian for Ed25519 scalars/points
- **Nonce randomness**: Must include fresh random `k` in rEdDSA
- **Adapter arithmetic**: `sig = sig_tilde + y` (Alice), not subtraction
- **Name shadowing**: Avoid variable names matching record accessors
- **Relude imports**: Many things auto-imported (IORef, Map, Set), check before adding imports
- **JSON ByteString**: Must write custom FromJSON/ToJSON (use hex encoding via Base16)
- **Safe list operations**: Use `viaNonEmpty head` instead of partial `head` (Relude pattern)
- **Orphan instances**: Keep all orphans in `AtomicSwap.Types.Orphans` module with `-Wno-orphans`
- **Formatting**: Never run individual formatters; treefmt runs automatically on commit
- **treefmt is source of truth**: All formatting configuration in treefmt.toml, never in individual tool configs

## Code Quality Standards

**EditorConfig**: `.editorconfig` enforces basic formatting across all editors

- 2-space indentation for Haskell
- 80-character line limit
- LF line endings, UTF-8 encoding

**Enhanced GHC Warnings**: Comprehensive warning flags catch bugs early

- `-Wunused-packages`: Identifies unnecessary dependencies
- `-Werror=missing-fields`: Record field omissions are errors
- `-fprint-explicit-foralls`: Clearer type error messages
- `-W identities, -Wredundant-bang-patterns`: Code quality checks

**treefmt Configuration** (`treefmt.toml` is the single source of truth):

- Orchestrates: fourmolu, cabal-fmt, nixfmt-rfc-style, prettier
- All fourmolu options passed as CLI args (no fourmolu.yaml)
- Key settings: `column-limit: 80`, `comma-style: leading`, `single-constraint-parens: never`
- Pre-commit hooks run treefmt (not individual formatters)

**Unicode Support** (Optional):

- Add `{-# LANGUAGE UnicodeSyntax #-}` to files that benefit from it
- Fourmolu will preserve unicode when extension is enabled
- Use for mathematical notation in crypto/protocol files if desired
- Example: `::` vs `∷`, `=>` vs `⇒`, `->` vs `→`

**Pre-commit Hooks** (Automatic):

- Runs on every commit automatically
- `treefmt` (formatting) + `hlint` (linting)
- Manual run: `pre-commit run --all-files`
- Configuration: `.pre-commit-config.yaml` (generated from flake.nix)
- Never run individual formatters; treefmt is the single entry point

---

## Self-Maintenance Reminder

**After completing any significant work** (new module, major refactoring, milestone):

1. Review this CLAUDE.md file
2. Update relevant sections if changes affect them
3. Add new patterns/pitfalls discovered during work
4. Update the Status line below with current progress

---

**Status**: Pure protocol implementation complete with io-classes (85% overall), QuickCheck + io-sim tests next
**Reference session**: Successful refactoring to functional core / imperative shell pattern
**Last updated**: 2025-11-14 (io-classes refactoring: MonadSTM abstraction for deterministic testing)

- Always re-format code after changing it with `treefmt`
