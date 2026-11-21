# The Nix devShells: `nix develop` for the extension example and the
# generators, `nix develop .#hosted` for the example that hosts Godot.

{ inputs, ... }:
{
  perSystem = { config, pkgs, system, ... }: {
    devShells.default = pkgs.mkShell {
      name = "gdextension-interface";
      meta.description = "Haskell + Godot development environment";

      # See https://community.flake.parts/haskell-flake/devshell#composing-devshells
      inputsFrom = [
        config.haskellProjects.default.outputs.devShell # See ./haskell.nix
        config.pre-commit.devShell # See ./pre-commit.nix
      ];

      # The project commands (example-run, ...) are defined in ./scripts.nix.
      packages = config.exampleScripts ++ (with pkgs; [
        nixd
        godot
        inputs.hs-bindgen.packages.${system}.hs-bindgen-cli
      ]);
    };

    # Building this shell builds Godot as a library, once: 15 to 35 minutes.
    devShells.hosted = pkgs.mkShell {
      name = "gdextension-interface-hosted";
      meta.description = "Haskell hosting Godot as a library";
      inputsFrom = [
        config.haskellProjects.hosted.outputs.devShell
        config.pre-commit.devShell
      ];
      packages = config.hostedScripts ++ [ pkgs.nixd ];
    };
  };
}
