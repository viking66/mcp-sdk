{
  description = "MCP - Model Context Protocol Haskell SDK";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    all-cabal-hashes = {
      url = "github:commercialhaskell/all-cabal-hashes/hackage";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, all-cabal-hashes }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        config = {
          allowUnfree = true;
          allowBroken = false;
        };

        pkgs = import nixpkgs {
          inherit system config;
          overlays = [ self.overlays.default ];
        };

        hsPkgs = pkgs.haskell.packages.ghc966.override {
          overrides = self: super: {
            mcp-core = self.callCabal2nix "mcp-core" ./mcp-core { };
            mcp-client = self.callCabal2nix "mcp-client" ./mcp-client { };
            mcp-server = self.callCabal2nix "mcp-server" ./mcp-server { };
          };
        };

      in {
        devShells.default = hsPkgs.shellFor {
          packages = p: [
            p.mcp-core 
            p.mcp-client 
            p.mcp-server
          ];

          withHoogle = true;

          nativeBuildInputs = with pkgs; [
            # GHC and core tools
            hsPkgs.ghc
            hsPkgs.cabal-install
            hsPkgs.cabal-fmt
            hsPkgs.fourmolu
            hsPkgs.hlint
            hsPkgs.hspec-discover
            hsPkgs.hoogle
            hsPkgs.haskell-language-server

            # Additional tools
            ghciwatch
            git
          ];

          buildInputs = with pkgs; [
            openssl
            pkg-config
            zlib
          ] ++ pkgs.lib.optional (system == "aarch64-darwin") pkgs.darwin.apple_sdk.frameworks.CoreServices;

          shellHook = ''
            echo "🔮 MCP Haskell SDK Development Environment"
            echo "Available tools:"
            echo "  - GHC: $(ghc --version)"
            echo "  - Cabal: $(cabal --version | head -n 1)"
            echo "  - Fourmolu: $(fourmolu --version)"
            echo "  - HLint: $(hlint --version)"
            echo "  - HLS: $(haskell-language-server-wrapper --version 2>/dev/null || echo 'not available')"
            echo "  - Hoogle: $(hoogle --version)"
            echo "  - Ghciwatch: $(ghciwatch --version)"
          '';
        };
      }
    ) // {
      overlays.default = final: prev: {
        inherit all-cabal-hashes;
      };
    };
}
