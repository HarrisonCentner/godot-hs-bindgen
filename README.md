# godot-hs-bindgen

Haskell bindings for Godot 4 via
[hs-bindgen](https://github.com/well-typed/hs-bindgen).
Either load Haskell as a Godot extension or let Haskell run Godot as a library.

```haskell
import Godot.Api
import Godot.Api.Class.Node qualified as Node
import Godot.Api.Class.Node3D qualified as Node3D

node <- Node3D.construct
Node3D.setPosition node (Vector3 1 2 3)
Vector3 {x} <- Node3D.getPosition node
Node.setName node =<< toStringName "from_haskell"   -- a Node method on a Node3D
parent <- Node3D.getParentNode3d node               -- Maybe Node3D
```

## Packages

| Package | What it is |
|---|---|
| `gdextension-interface` | Godot's C interface, by hs-bindgen, plus a typed record of its functions |
| `godot-api` | The engine's classes, value types, enums and variants |
| `godot-host` | Runs Godot inside a Haskell program |

## What the engine API gives you

- A `newtype` per class; methods accept any subclass.
- Value types (`Vector3`, `Color`, `Transform3D`, ...) as strict records.
- Variants with a closed witness of their type: `toVariant`, a checked
  `fromVariant`, and `viewVariant` to pattern match on.
- Every method as `safe` and `_unsafe` foreign call 

## Examples

**Godot hosts Haskell** (`examples/extension`): Godot loads a Haskell shared
library as an extension.

```sh
nix develop
example-run --headless --quit   # build it, load it in Godot, exit
example-run                     # same, with a window
test-run                        # the hedgehog suite, inside Godot
```

**Haskell hosts Godot** (`examples/hosted`): a Haskell program drives the
frame loop and turns a cube before every frame.

```sh
nix develop .#hosted
hosted-run                          # a window; close it to exit
hosted-run --frames 120 --headless
```

```haskell
main = withGodot ["my-program", "--path", "project"] $ \godot ->
  let loop = do
        -- per-frame Haskell code
        quit <- GodotInstance.iteration godot
        unless quit loop
   in loop
```

## Using it from another flake

`lib.mkBindings` builds the packages against your own nixpkgs. Pass `classes`
to generate methods for those classes and their ancestors only.
With [haskell-flake](https://github.com/srid/haskell-flake):

```nix
perSystem = { pkgs, ... }:
  let
    bindings = inputs.godot-hs-bindgen.lib.mkBindings {
      inherit pkgs;
      classes = [ "Node3D" "MeshInstance3D" ];
    };
  in
  {
    haskellProjects.default = {
      packages = {
        gdextension-interface.source = bindings.gdextension-bindings.outPath;
        godot-api.source = bindings.godot-api-src.outPath;
        hs-bindgen-runtime.source = bindings.hs-bindgen-runtime-src;
      };
      settings = {
        hs-bindgen-runtime.check = false;
        # cabal2nix ignores a foreign-library's build-depends.
        my-extension = { self, ... }: {
          extraBuildDepends = [ self.gdextension-interface self.godot-api self.hs-bindgen-runtime ];
        };
      };
    };
  };
```

An extension also needs a C shim that starts the GHC runtime and a
`.gdextension` manifest; copy both from `examples/extension`.

## Acknowledgments

The design was largely influenced by
[DavidEichmann/godot-hs](https://github.com/DavidEichmann/godot-hs).

## License

MIT; see [LICENSE](LICENSE).
