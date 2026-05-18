{
  description = "Agda MCP";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    crane.url = "github:ipetkov/crane";

    fenix.url = "github:nix-community/fenix";
    fenix.inputs.nixpkgs.follows = "nixpkgs";

    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      crane,
      fenix,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        cargoToml = builtins.fromTOML (builtins.readFile ./Cargo.toml);
        packageName = cargoToml.package.name;

        toolchain = fenix.packages.${system}.stable.toolchain;
        craneLib = (crane.mkLib pkgs).overrideToolchain toolchain;

        src = craneLib.cleanCargoSource ./.;
        commonArgs = {
          inherit src;
          strictDeps = true;
        };
        cargoArtifacts = craneLib.buildDepsOnly commonArgs;

        package = craneLib.buildPackage (
          commonArgs
          // {
            inherit cargoArtifacts;
            # Keep package builds lightweight for downstream dev shells. The
            # integration tests in tests/ spawn Agda, so run only crate-local
            # tests here and exercise the full suite in checks.default below.
            cargoTestExtraArgs = "--lib --bins";
            meta.mainProgram = packageName;
          }
        );
      in
      {
        packages = {
          default = package;
        }
        // {
          ${packageName} = package;
        };

        apps.default = {
          type = "app";
          program = "${package}/bin/${packageName}";
          meta.description = "Run ${packageName}";
        };

        checks = {
          default = craneLib.cargoTest (
            commonArgs
            // {
              inherit cargoArtifacts;
              nativeCheckInputs = [ pkgs.agda ];
            }
          );

          clippy = craneLib.cargoClippy (
            commonArgs
            // {
              inherit cargoArtifacts;
              cargoClippyExtraArgs = "--all-targets -- --deny warnings";
            }
          );

          fmt = craneLib.cargoFmt { inherit src; };
        };

        devShells.default = craneLib.devShell {
          checks = self.checks.${system};
          packages = with pkgs; [
            agda
            cargo-edit
            cargo-machete
            rust-analyzer
          ];
        };
      }
    );
}
