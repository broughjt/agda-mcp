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
        hackage =
          self: pkg: ver: sha256:
          self.callHackageDirect { inherit pkg ver sha256; };
        haskellPackages = pkgs.haskellPackages.override {
          overrides = self: super: {
            # nixpkgs ships mcp-server 0.1, but we need the 0.3 API.
            mcp-server = hackage self "mcp-server" "0.3.0.1" "sha256-SFPX03m6UhuyQfnoxiiRSegZp7tkhZ0zeS/GGJtC1U4=" {
              # mcp-server needs crypton >= 1.1.4; nixpkgs ships 1.0.6.
              crypton = hackage self "crypton" "2.2.6" "sha256-Ll/UXRpvON64iincSgzFkcIA9CK9EeAygcjveYTlVoU=" { };
            };
            # warp's `x509` flag pulls in crypton-x509, which needs crypton < 1.1
            # and so would put a second crypton beside mcp-server's. We do not
            # serve HTTPS.
            warp = pkgs.haskell.lib.disableCabalFlag super.warp "x509";
            # mcp-server's HTTP transport uses `hOrigin`, which nixpkgs'
            # http-types 0.12.4 predates. We do not serve HTTP, but the module
            # is still part of the library and has to compile.
            http-types = pkgs.haskell.lib.dontCheck (
              hackage self "http-types" "0.12.6" "sha256-bGrVUTZnP1NFVwR0apgNLolKPW3cJdVW4jSBSlV7srg=" { }
            );
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
