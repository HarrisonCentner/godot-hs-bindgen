# Project commands, available on PATH inside the devShells.
# Each one can be run from any directory in the repository.

{ ... }:
{
  perSystem = { pkgs, lib, ... }:
    let
      mkScript = dir: name: text: pkgs.writeShellApplication {
        inherit name;
        text = ''
          cd "$(git rev-parse --show-toplevel)/examples/${dir}"
          ${text}
        '';
      };
      scripts = dir: defs: lib.attrValues (lib.mapAttrs (mkScript dir) defs);
      scriptList = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        description = "Project commands to add to a devShell.";
      };
    in
    {
      options.exampleScripts = scriptList;
      options.hostedScripts = scriptList;

      # The default devShell: Godot loads the Haskell extension.
      config.exampleScripts = scripts "extension" {
        # Build the example extension and copy it into the Godot project.
        example-ext = ''
          cabal build flib:hs_example
          lib=$(find dist-newstyle -name libhs_example.so -path '*/f/hs_example/*' | head -n1)
          cp -f "$lib" godot/bin/libhs_example.so
        '';

        # Build the example and run the Godot project. Arguments go to Godot.
        example-run = ''
          example-ext
          godot --path godot --headless --import
          godot --path godot "$@"
        '';

        # Build the test extension and run the hedgehog suite inside Godot.
        test-run = ''
          cabal build flib:hs_tests
          lib=$(find dist-newstyle -name libhs_tests.so -path '*/f/hs_tests/*' | head -n1)
          cp -f "$lib" godot-tests/bin/libhs_tests.so
          godot --path godot-tests --headless --import > /dev/null 2>&1
          godot --path godot-tests --headless --quit
        '';

        # Build the example and open the Godot editor.
        example-edit = ''
          example-ext
          godot --path godot --editor
        '';
      };

      # The hosted devShell: a Haskell program with Godot as a library.
      config.hostedScripts = scripts "hosted" {
        # Arguments go to the program (--frames N) and on to Godot.
        hosted-run = ''
          cabal run -v0 exe:hosted-example -- --path godot "$@"
        '';
      };
    };
}
