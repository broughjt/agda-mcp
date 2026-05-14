{
  description = "Agda MCP server";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        mcpOverlay = final: prev: {
          haskellPackages = prev.haskellPackages.override {
            overrides = hfinal: _hprev: {
              # Some nixpkgs revisions still package an older/broken mcp.
              # Pin the Hackage release this server is written against so
              # downstream flakes can make agda-mcp.inputs.nixpkgs follow
              # their own nixpkgs without depending on that package set's
              # mcp version.
              jsonrpc = hfinal.callHackageDirect {
                pkg = "jsonrpc";
                ver = "0.2.0.0";
                sha256 = "sha256-j9Xb3jlFlLN+XTxxgqI43tpB31PNZvNOu0h+8JzEVYo=";
              } { };

              mcp-types = hfinal.callHackageDirect {
                pkg = "mcp-types";
                ver = "0.1.1";
                sha256 = "sha256-ZYx+R5PSLCD+rL0ZC0aOI523LvWxXp1LcVG1opEs9sc=";
              } { };

              mcp = prev.haskell.lib.markUnbroken (hfinal.callHackageDirect {
                pkg = "mcp";
                ver = "0.3.1.0";
                sha256 = "sha256-TA31PP7X0tQFO8uLc0ptCxLObOPCJFCqxBusnBywhrk=";
              } { });
            };
          };
        };

        pkgs = import nixpkgs {
          inherit system;
          overlays = [ mcpOverlay ];
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
