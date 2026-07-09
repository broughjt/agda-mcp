{
  description = "Agda MCP";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        mcpSrc = pkgs.fetchFromGitHub {
          owner = "dpella";
          repo = "mcp";
          rev = "151286f7c457d806c57801ba2bb2d2e614ee8e55";
          hash = "sha256-RGUh1FiLX4hLwzMR2U/LLInYLzqVMAwgOZy2CD6vwns=";
        };
        haskellPackages = pkgs.haskellPackages.override {
          overrides = self: _: {
            mcp = self.callCabal2nix "mcp" (mcpSrc + "/mcp-server") { };
          };
        };
        packageName = "agda-mcp";
        package = haskellPackages.callCabal2nix packageName ./. { };
      in
      {
        packages = {
          default = package;
          ${packageName} = package;
        };

        apps.default = {
          type = "app";
          program = "${package}/bin/${packageName}";
          meta.description = "Run ${packageName}";
        };

        devShells.default = haskellPackages.shellFor {
          packages = _: [ package ];

          buildInputs = [
            pkgs.cabal-install
            pkgs.haskell-language-server
            pkgs.fourmolu
            pkgs.hlint
            pkgs.ghcid
            pkgs.zlib
          ];
        };
      }
    );
}
