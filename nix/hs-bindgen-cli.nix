# hs-bindgen-cli, built from the hs-bindgen flake's own nixpkgs pin and
# overlay, as its `packages.hs-bindgen-cli` is, with one change:
# c-expr-runtime is built without its documentation.
#
# No binary cache has hs-bindgen, so every CI run compiles it. Haddock on
# c-expr-runtime sometimes never returns: it prints its last warning and then
# stays silent until the job reaches GitHub's six-hour limit. hs-bindgen
# already skips Haddock for hs-bindgen and doxygen-parser; nothing here reads
# the documentation of any of them.
{ hs-bindgen, system }:
let
  noHaddock = final: prev: {
    haskell = prev.haskell // {
      packageOverrides = final.lib.composeExtensions prev.haskell.packageOverrides (
        _: hprev: {
          c-expr-runtime = final.haskell.lib.compose.dontHaddock hprev.c-expr-runtime;
        }
      );
    };
  };
  pkgs = import hs-bindgen.inputs.nixpkgs {
    inherit system;
    overlays = [ hs-bindgen.overlays.default noHaddock ];
  };
in
pkgs.hs-bindgen-cli
