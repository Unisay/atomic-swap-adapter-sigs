{
  description = "Educational atomic swap tutorial using adapter signatures";

  inputs = {
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackage";
    };

    nixpkgs.follows = "haskell-nix/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";

    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      haskell-nix,
      hackage,
      pre-commit-hooks,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        # Import nixpkgs with haskell.nix overlays
        pkgs = import nixpkgs {
          inherit system;
          config = haskell-nix.config;
          overlays = [ haskell-nix.overlay ];
        };

        # haskell.nix project for building the Cabal project
        project = pkgs.haskell-nix.cabalProject' {
          src = ./.;
          compiler-nix-name = "ghc966";

          modules = [
            {
              packages.atomic-swap-adapter-sigs.package.buildable = true;
            }
          ];
        };

        # Extract components from the project
        atomicSwapLib = project.hsPkgs.atomic-swap-adapter-sigs.components.library;
        atomicSwapTests = project.hsPkgs.atomic-swap-adapter-sigs.components.tests.atomic-swap-tests;

        # Pre-commit hooks configuration
        # Note: Formatting is handled by treefmt, not individual pre-commit hooks
        pre-commit-check = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            # treefmt handles all formatting (fourmolu, cabal-fmt, nixfmt, prettier)
            treefmt = {
              enable = true;
              package = pkgs.treefmt;
              settings.formatters = [
                pkgs.fourmolu
                pkgs.haskellPackages.cabal-fmt
                pkgs.nixfmt-rfc-style
                pkgs.nodePackages.prettier
              ];
            };
            # hlint is the only hook that doesn't modify files
            hlint.enable = true;
          };
        };

        # Development shell
        baseShell = project.shellFor {
          # Include all local packages in the shell
          packages = p: [ p.atomic-swap-adapter-sigs ];

          # Haskell development tools
          tools = {
            cabal = "latest";
            haskell-language-server = "latest";
          };

          # Build tools and dependencies
          buildInputs =
            with pkgs;
            [
              # Haskell development tools
              fourmolu
              haskellPackages.cabal-fmt
              haskellPackages.hlint

              # Core development tools
              git

              # Formatting tools
              treefmt
              nodePackages.prettier # Markdown
              nixfmt-rfc-style

              # Documentation
              mdbook

              # Development workflow
              watchman # File watching for auto-rebuild

              # Essential utilities
              jq

              # Cryptographic libraries
              libsodium
              pkg-config

              # System libraries
              zlib
            ]
            ++ pre-commit-check.enabledPackages;

          shellHook = ''
            ${pre-commit-check.shellHook}

            echo "🔐 Atomic Swap Tutorial Development Environment"
            echo ""
            echo "Commands:"
            echo "  cabal build                     # Build the library"
            echo "  cabal test                      # Run tests"
            echo "  cabal run atomic-swap-simulator # Run simulator server"
            echo "  pre-commit run --all-files      # Run all pre-commit hooks"
            echo "  treefmt                         # Format all code"
            echo "  hlint src/ test/                # Lint code"
            echo ""
            echo "Development workflow:"
            echo "  See CLAUDE.md for watchman-make auto-rebuild commands"
            echo ""
          '';
        };
      in
      {
        # Expose packages for building
        packages = {
          default = atomicSwapLib;
          atomic-swap-lib = atomicSwapLib;
        };

        # Expose checks for CI
        checks = {
          atomic-swap-tests = atomicSwapTests;
          pre-commit = pre-commit-check;
        };

        # Development shell
        devShells.default = baseShell;
      }
    );

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
    allow-import-from-derivation = true;
    accept-flake-config = true;
  };
}
