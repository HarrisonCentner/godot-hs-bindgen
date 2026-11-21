# haskell-flake configuration goes in this module.
#
# There are two projects, one per example, so that the half-hour build of
# Godot as a library is only needed by the one that hosts it:
#
#   default  examples/extension  Godot loads Haskell; uses the nixpkgs Godot
#   hosted   examples/hosted     Haskell runs Godot; needs libgodot

{ inputs, ... }:
let
  # The repository root.
  root = ../../..;
in
{
  imports = [
    inputs.haskell-flake.flakeModule
  ];
  perSystem = { self', config, pkgs, ... }:
    let
      # The examples only need a few classes; generating all of them works
      # (see packages.godot-api-full) but takes much longer to compile.
      bindings = inputs.self.lib.mkBindings {
        inherit pkgs;
        classes = [ "Node3D" "MeshInstance3D" "BoxMesh" "Camera3D" "DirectionalLight3D" "SceneTree" "Window" "Engine" "GodotInstance" "OS" ];
      };
      allClasses = inputs.self.lib.mkBindings { inherit pkgs; };

      # The generated packages both projects build on.
      generated = {
        # The C interface, generated at build time (see ../../godot-bindings.nix).
        gdextension-interface.source = bindings.gdextension-bindings.outPath;
        # The engine class API on top of it, also generated.
        godot-api.source = bindings.godot-api-src.outPath;
        # Not in nixpkgs; taken from the same checkout as the generator so
        # the generated module and its runtime always match.
        hs-bindgen-runtime.source = bindings.hs-bindgen-runtime-src;
      };

      extension = config.haskellProjects.default.outputs.finalPackages;
      hosted = config.haskellProjects.hosted.outputs.finalPackages;
    in
    {
      haskellProjects.default = {
        projectRoot = root + /examples/extension;
        basePackages = pkgs.haskellPackages;
        packages = generated;
        settings = {
          hs-bindgen-runtime.check = false;
          # cabal2nix does not pick up a foreign-library's dependencies, so
          # they are listed here to reach the build and the devShell.
          example-extension = { self, ... }: {
            extraBuildDepends = [
              self.gdextension-interface
              self.godot-api
              self.hedgehog
              self.hs-bindgen-runtime
            ];
          };
        };
        autoWire = [ "packages" ];
      };

      haskellProjects.hosted = {
        projectRoot = root + /examples/hosted;
        basePackages = pkgs.haskellPackages;
        packages = generated // {
          # Hosting Godot as a library; links libgodot.
          godot-host.source = bindings.godot-host-src.outPath;
        };
        settings = {
          hs-bindgen-runtime.check = false;
          # cabal2nix resolves `extra-libraries: godot` to nixpkgs' Godot
          # editor; the library build is what has libgodot.so.
          godot-host.custom = pkg: pkg.override { godot = bindings.libgodot; };
        };
        autoWire = [ "packages" "apps" ];
      };

      # The generated libraries. haskell-flake only wires up local packages,
      # and these are dependencies of the examples, so they are exposed by
      # hand.
      packages.gdextension-interface = extension.gdextension-interface;
      packages.godot-api = extension.godot-api;
      packages.godot-host = hosted.godot-host;
      # haskell-flake prefixes the second project's outputs with its name;
      # give the hosted example a plain one too.
      packages.example-hosted = hosted.example-hosted;

      # The engine API with method modules for every class: about a thousand
      # modules, where godot-api above has only what the examples use.
      packages.godot-api-full = extension.callCabal2nix "godot-api" allClasses.godot-api-src.outPath { };

      packages.default = self'.packages.gdextension-interface;
    };
}
