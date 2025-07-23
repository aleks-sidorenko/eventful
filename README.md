# Eventful

A library for building event sourcing systems in Haskell.

## Build System

This project uses **Nix + Cabal** for reproducible builds with GHC 9.6.7.

### Quick Start

```bash
# Enter development environment
nix develop

# Build all packages
cabal build all

# Run tests
cabal test eventful-core eventful-memory
```

### Automated Build & Test

Use the provided script for a complete build and test cycle:

```bash
# Run comprehensive build and test
./scripts/test-build.sh
```

This script will:
- Enter the Nix development environment
- Regenerate cabal files from package.yaml
- Build core packages
- Run all tests
- Provide a summary of applied fixes

### Available Commands (in nix develop)

- `cabal build` - Build all packages
- `cabal test` - Run all tests
- `hpack` - Generate cabal files from package.yaml
- `ghcid` - Continuous compilation

### Database Tools

- `psql` - PostgreSQL client
- `sqlite3` - SQLite client

## Project Status

✅ **Working packages**:
- eventful-core - Core event sourcing library (with Template Haskell support)
- eventful-memory - In-memory implementations
- eventful-test-helpers - Testing utilities

⚠️ **Temporarily disabled** (due to dependency compatibility):
- SQL backends (postgresql, sqlite)
- Some examples and tutorials
