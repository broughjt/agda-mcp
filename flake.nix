{
  description = "Agda MCP server";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        mcpUnbrokenOverlay = final: prev: {
          haskellPackages = prev.haskellPackages.override {
            overrides = hfinal: hprev: {
              # nixpkgs still marks haskellPackages.mcp as broken from an
              # older Hydra failure, but the current version builds and its
              # stdio transport tests pass.
              mcp = prev.haskell.lib.markUnbroken hprev.mcp;
            };
          };
        };

        pkgs = import nixpkgs {
          inherit system;
          overlays = [ mcpUnbrokenOverlay ];
        };

        hs = pkgs.haskellPackages;

        haskellDeps = hpkgs: [
          hpkgs.Agda
          hpkgs.aeson
          hpkgs.containers
          hpkgs.mcp
          hpkgs.text
        ];

        ghcWithDeps = hs.ghcWithPackages haskellDeps;

        agda-mcp = hs.callPackage
          ({ Agda
           , aeson
           , base
           , containers
           , lib
           , mcp
           , text
           }:
            hs.mkDerivation {
              pname = "agda-mcp";
              version = "0.1.0.0";
              src = lib.cleanSource ./.;
              isLibrary = false;
              isExecutable = true;
              executableHaskellDepends = [
                Agda
                aeson
                base
                containers
                mcp
                text
              ];
              description = "Agda MCP server";
              license = lib.licenses.bsd3;
              mainProgram = "agda-mcp";
            }) { };

        devTools = [
          ghcWithDeps
          pkgs.cabal-install
          pkgs.haskell-language-server
          pkgs.fourmolu
          pkgs.hlint
          pkgs.ghcid
        ];
      in
      {
        packages = {
          default = agda-mcp;
          agda-mcp = agda-mcp;
        };

        apps.default = {
          type = "app";
          program = "${agda-mcp}/bin/agda-mcp";
        };

        devShells.default = pkgs.mkShell {
          packages = devTools;
        };
      });
}
