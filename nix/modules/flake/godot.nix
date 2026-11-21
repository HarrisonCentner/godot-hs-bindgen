# Packages and checks derived from the Godot engine; see
# ../../godot-bindings.nix.

{ inputs, ... }:
let
  root = ../../..;
in
{
  perSystem = { self', pkgs, ... }:
    let
      bindings = inputs.self.lib.mkBindings { inherit pkgs; };
    in
    {
      packages = {
        inherit (bindings) godot-api-json gdextension-bindings godot-api-src godot-host-src;

        # The engine as a shared library, for hosting it from Haskell.
        inherit (bindings) libgodot;

        # The example extension's shared library on its own, at a stable path.
        example-ext = pkgs.runCommand "example-extension-lib" { } ''
          mkdir -p $out/lib
          cp ${self'.packages.example-extension}/lib/ghc-*/lib/libhs_example.so $out/lib/
        '';
      };

      # The test suite: hedgehog properties for the engine API, run inside
      # headless Godot, since the API needs a running engine.
      checks.engine-tests = pkgs.runCommand "engine-tests"
        {
          nativeBuildInputs = [ pkgs.godot ];
        } ''
        export HOME=$TMPDIR
        cp -r ${root + /examples/extension/godot-tests} project
        chmod -R u+w project
        mkdir -p project/bin
        cp ${self'.packages.example-extension}/lib/ghc-*/lib/libhs_tests.so project/bin/
        godot --path project --headless --import > /dev/null 2>&1
        godot --path project --headless --quit 2>&1 | tee log
        grep -q "\[hs-tests\] PASSED" log
        # Godot reports objects and resources still alive when it exits.
        ! grep -i "leaked at exit" log
        touch $out
      '';

      # Loads the example extension in headless Godot and checks what it
      # prints: an end-to-end test of the generated bindings against the
      # engine they were generated from.
      checks.extension-example = pkgs.runCommand "extension-example-runs"
        {
          nativeBuildInputs = [ pkgs.godot ];
        } ''
        export HOME=$TMPDIR
        cp -r ${root + /examples/extension/godot} project
        chmod -R u+w project
        mkdir -p project/bin
        cp ${self'.packages.example-ext}/lib/libhs_example.so project/bin/
        godot --path project --headless --import
        godot --path project --headless --quit 2>&1 | tee log
        grep -q "position: Vector3 {x = 1.0, y = 2.0, z = 3.0}" log
        grep -q "name: from_haskell" log
        grep -q "child's parent is the node: True" log
        ! grep -i "leaked at exit" log
        touch $out
      '';
    };
}
