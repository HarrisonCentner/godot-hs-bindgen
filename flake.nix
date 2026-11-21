{
  description = "Haskell bindings for Godot's GDExtension C interface, generated with hs-bindgen";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    haskell-flake.url = "github:srid/haskell-flake";
    fourmolu-nix.url = "github:jedimahdi/fourmolu-nix";

    git-hooks.url = "github:cachix/git-hooks.nix";
    git-hooks.flake = false;

    # Generates the bindings from Godot's C header. Keeps its own nixpkgs
    # pin, since its libclang overrides are tested against that.
    hs-bindgen.url = "github:well-typed/hs-bindgen";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];

      imports = [
        ./nix/modules/flake/haskell.nix
        ./nix/modules/flake/godot.nix
        ./nix/modules/flake/scripts.nix
        ./nix/modules/flake/devshell.nix
        ./nix/modules/flake/pre-commit.nix
      ];

      # For other flakes: builds the bindings against the caller's nixpkgs,
      # so that the Godot version and the Haskell package set are theirs.
      #
      #   bindings = inputs.gdextension-interface.lib.mkBindings { inherit pkgs; };
      #
      # Pass `classes = [ "Node3D" ... ]` to generate method modules for those
      # classes and their ancestors only, which shortens the build a great deal.
      #
      # Returns:
      #   godot-api-json          the dumped C header and API JSON files
      #   gdextension-bindings    source of the gdextension-interface package
      #   godot-api-src           source of the godot-api package (engine classes)
      #   hs-bindgen-runtime-src  source of the matching hs-bindgen-runtime
      flake.lib.mkBindings = { pkgs, classes ? [ ] }:
        import ./nix/godot-bindings.nix
          {
            inherit pkgs classes;
            inherit (inputs.hs-bindgen.packages.${pkgs.stdenv.hostPlatform.system}) hs-bindgen-cli;
          } // {
          hs-bindgen-runtime-src = inputs.hs-bindgen + /hs-bindgen-runtime;
        };
    };
}
