{
  description = "Agda MCP server";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        hs = pkgs.haskellPackages;

        haskellDeps = hpkgs: [
          hpkgs.Agda
          hpkgs.aeson
          hpkgs.bytestring
          hpkgs.containers
          hpkgs.mcp-types
          hpkgs.text
        ];

        ghcWithDeps = hs.ghcWithPackages haskellDeps;

        agda-mcp = hs.callPackage
          ({ Agda
           , aeson
           , base
           , bytestring
           , containers
           , lib
           , mcp-types
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
                bytestring
                containers
                mcp-types
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
