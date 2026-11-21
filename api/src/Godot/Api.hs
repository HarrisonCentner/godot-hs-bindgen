{- | Godot's engine API: classes, builtin value types and enums.

Call 'initialize' from the extension's entry point. The methods of each
class are in @Godot.Api.Class.\<Name\>@, meant to be imported qualified:

> import Godot.Api
> import qualified Godot.Api.Class.Node3D as Node3D
>
> node <- Node3D.construct
> Node3D.setPosition node (Vector3 1 2 3)

Methods accept any subclass of the class they belong to. Every method also
has an @_unsafe@ variant, which is faster but must not be used when the
engine may call back into Haskell during the call.

Objects returned by the engine are 'Maybe' unless Godot declares them
non-null. Reference-counted objects returned from a method carry a
reference that is not released automatically.
-}
module Godot.Api (
  -- * Initialization
  initialize,

  -- * Objects
  GodotClass,
  Inherits,
  upcast,
  cast,
  nullObject,
  isAlive,
  freeObject,
  ObjectFreed (..),
  releasePending,

  -- * Strings
  module Godot.Api.Strings,

  -- * Variants
  Variant,
  newDefault,
  module Godot.Api.Variant,

  -- * Generated types
  module Godot.Api.Builtins,
  module Godot.Api.Classes,
  module Godot.Api.Enums,
) where

import Godot.Api.Builtins
import Godot.Api.Classes
import Godot.Api.Enums
import Godot.Api.Runtime
import Godot.Api.Strings
import Godot.Api.Variant
