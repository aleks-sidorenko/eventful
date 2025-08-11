{
  description = "Eventful - A library for building event sourcing systems";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        # Use GHC 9.6.7 as requested
        hPkgs = pkgs.haskell.packages.ghc967;
        
        # Define all the local packages
        localPackages = [
          "eventium-core"
          "eventium-memory"
          "eventium-postgresql"
          "eventium-sql-common"
          "eventium-sqlite"
          "eventium-test-helpers"          
          "examples/bank"
          "examples/cafe"
          "examples/counter-cli"
        ];

        # Function to build a single package
        buildPackage = name: 
          hPkgs.callCabal2nix (builtins.replaceStrings ["/"] ["-"] name) ./${name} {};

        # Build all packages
        packages = builtins.listToAttrs (map (name: {
          name = builtins.replaceStrings ["/"] ["-"] name;
          value = buildPackage name;
        }) localPackages);

        # Development dependencies
        devDependencies = with pkgs; [
          # Haskell tools
          hPkgs.ghc
          hPkgs.cabal-install
          hPkgs.hpack
          stack
          
          # Database libraries/headers for building (no local server needed)
          postgresql.lib
          postgresql.dev
          sqlite
          
          # Development tools
          hPkgs.haskell-language-server
          hPkgs.hlint
          hPkgs.ormolu
          hPkgs.ghcid
          hPkgs.hspec-discover
          
          # System dependencies
          pkg-config
          zlib
        ];

      in {
        # Export all packages
        packages = packages // {
          default = packages.eventium-core;
          
          # Combined package that builds everything
          all = hPkgs.buildStackProject {
            name = "eventium-all";
            src = ./.;
            ghc = hPkgs.ghc;
            buildInputs = with pkgs; [
              postgresql.lib
              postgresql.dev
              sqlite
              zlib
            ];
          };
        };

        # Development shell
        devShells.default = pkgs.mkShell {
          buildInputs = devDependencies;
          
          # Set up environment for development
          shellHook = ''
            echo "🎯 Eventful Development Environment"
            echo "📦 GHC version: $(ghc --version)"
            echo "🔧 Available commands:"
            echo "  • cabal build          - Build all packages"
            echo "  • cabal test           - Run all tests"
            echo "  • hpack                - Generate cabal files from package.yaml"
            echo "  • ghcid                - Continuous compilation"
            echo ""
            echo "🗄️  Database libraries available:"
            echo "  • libpq (from postgresql.lib)"
            echo "  • sqlite3"
            echo ""
            
            # Generate cabal files from package.yaml files
            echo "📝 Generating cabal files from package.yaml..."
            find . -name package.yaml -exec dirname {} \; | while read dir; do
              echo "  → $dir"
              (cd "$dir" && hpack)
            done
            echo "✅ Cabal files generated"

            # Optionally start a local PostgreSQL if running inside a container/CI
            if [ "''${IN_CONTAINER:-}" = "1" ]; then
              export POSTGRES_HOST=''${POSTGRES_HOST:-127.0.0.1}
              export POSTGRES_PORT=''${POSTGRES_PORT:-5432}
              export POSTGRES_USER=''${POSTGRES_USER:-postgres}
              export POSTGRES_PASSWORD=''${POSTGRES_PASSWORD:-password}
              export POSTGRES_DBNAME=''${POSTGRES_DBNAME:-eventium_test}
              echo "Ensuring local PostgreSQL is running (IN_CONTAINER=1)..."
              bash scripts/start-postgres.sh || true
            fi
          '';

          # Environment variables
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
            pkgs.postgresql.lib
            pkgs.sqlite
            pkgs.zlib
          ];
        };

        # Apps for easy running
        apps = {
          default = flake-utils.lib.mkApp {
            drv = packages.eventium-core;
          };
          
          # Example apps
          bank-example = flake-utils.lib.mkApp {
            drv = packages.examples-bank;
          };
          
          counter-cli = flake-utils.lib.mkApp {
            drv = packages.examples-counter-cli;
          };
        };

      });
} 