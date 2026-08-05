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
        standardLibrary = pkgs.agdaPackages.standard-library;
        packageName = "agda-mcp";
        # The test suite resolves the pinned standard library through
        # AGDA_MCP_STDLIB, so the check phase needs it exactly as the dev shell
        # does. Without it every case fails on a missing library.
        unwrapped = pkgs.haskell.lib.overrideCabal (haskellPackages.callCabal2nix packageName ./. { }) (
          drv: {
            preCheck = (drv.preCheck or "") + ''
              export AGDA_MCP_STDLIB=${standardLibrary}
            '';
          }
        );
        # Mirrors `agda.withPackages` by cooking up a libraries file listing the
        # selected libraries and then passing `--library-file` in a wrapper.
        withPackages =
          selection:
          let
            selected = if builtins.isList selection then selection else selection pkgs.agdaPackages;
            libraries = pkgs.writeText "libraries" (
              pkgs.lib.concatMapStringsSep "\n" (library: "${library}/${library.libraryFile}") selected
            );
          in
          pkgs.runCommand "${packageName}-with-packages"
            {
              nativeBuildInputs = [ pkgs.makeWrapper ];
              meta.mainProgram = packageName;
            }
            ''
              mkdir -p $out/bin
              makeWrapper ${unwrapped}/bin/${packageName} $out/bin/${packageName} \
                --add-flags "--library-file=${libraries}"
            '';
        package = unwrapped // { inherit unwrapped withPackages; };
      in
      {
        packages = {
          default = package;
          ${packageName} = package;
        };

        checks.${packageName} = unwrapped;

        apps.default = {
          type = "app";
          program = "${package}/bin/${packageName}";
          meta.description = "Run ${packageName}";
        };

        devShells.default = haskellPackages.shellFor {
          packages = _: [ unwrapped ];

          AGDA_MCP_STDLIB = standardLibrary;

          buildInputs = [
            pkgs.cabal-install
            pkgs.haskell-language-server
            pkgs.fourmolu
            pkgs.hlint
            pkgs.ghcid
            pkgs.zlib
            haskellPackages.tasty
            haskellPackages.tasty-hunit
            haskellPackages.temporary
          ];
        };
      }
    );
}
