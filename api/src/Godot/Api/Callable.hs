{-# LANGUAGE ForeignFunctionInterface #-}

{- | Godot @Callable@s that run Haskell code: what a signal is connected to.

> pressed <- toStringName "pressed"
> onPressed <- newCallable_ (\_ -> putStrLn "pressed")
> _ <- Object.connect button pressed onPressed 0
-}
module Godot.Api.Callable (
  newCallable,
  newCallable_,
) where

import Control.Exception (SomeException, try)
import Foreign
import Godot.Api.Builtins (Callable)
import Godot.Api.Runtime (Variant)
import Godot.Api.Runtime qualified as R
import Godot.GDExtension.Functions (Functions (..))
import Godot.GDExtension.Interface
import HsBindgen.Runtime.PtrConst qualified as PtrConst
import HsBindgen.Runtime.Support.FunPtr (toFunPtr)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

{- | A callable that runs the function with the arguments it is called with
and returns its result.

Godot calls it on whatever thread emits the signal or makes the call. The
function is kept alive for as long as Godot holds the callable. An exception
it throws is reported on stderr and the call returns nil.
-}
newCallable :: ([Variant] -> IO Variant) -> IO Callable
newCallable function = do
  userdata <- newStablePtr function
  R.newOpaqueWith $ \callable ->
    with
      GDExtensionCallableCustomInfo2
        { gDExtensionCallableCustomInfo2_callable_userdata = castPtr (castStablePtrToPtr userdata)
        , gDExtensionCallableCustomInfo2_token = let GDExtensionClassLibraryPtr p = R.library in p
        , gDExtensionCallableCustomInfo2_object_id = GDObjectInstanceID 0
        , gDExtensionCallableCustomInfo2_call_func = onCall
        , gDExtensionCallableCustomInfo2_is_valid_func = GDExtensionCallableCustomIsValid nullFunPtr
        , gDExtensionCallableCustomInfo2_free_func = onFree
        , gDExtensionCallableCustomInfo2_hash_func = GDExtensionCallableCustomHash nullFunPtr
        , gDExtensionCallableCustomInfo2_equal_func = GDExtensionCallableCustomEqual nullFunPtr
        , gDExtensionCallableCustomInfo2_less_than_func = GDExtensionCallableCustomLessThan nullFunPtr
        , gDExtensionCallableCustomInfo2_to_string_func = GDExtensionCallableCustomToString nullFunPtr
        , gDExtensionCallableCustomInfo2_get_argument_count_func = GDExtensionCallableCustomGetArgumentCount nullFunPtr
        }
      (callableCustomCreate2 R.functions (GDExtensionUninitializedTypePtr (castPtr callable)))

-- | 'newCallable' for a function with no result.
newCallable_ :: ([Variant] -> IO ()) -> IO Callable
newCallable_ function = newCallable (\arguments -> function arguments >> R.nilVariant)

-- The two entry points Godot uses for every Haskell callable; the callable's
-- own function is the userdata. They live as long as the library.

onCall :: GDExtensionCallableCustomCall
onCall = GDExtensionCallableCustomCall . unsafePerformIO . toFunPtr . GDExtensionCallableCustomCall_Aux $
  \userdata arguments (GDExtensionInt count) (GDExtensionVariantPtr result) callError -> do
    function <- deRefStablePtr (castPtrToStablePtr (castPtr userdata)) :: IO ([Variant] -> IO Variant)
    -- The arguments belong to the caller; take copies the function can keep.
    pointers <- peekArray (fromIntegral count) (castPtr (PtrConst.unsafeToPtr arguments)) :: IO [Ptr ()]
    outcome <- try (traverse R.copyVariant pointers >>= function)
    case outcome of
      Right value -> R.assignVariant (castPtr result) value
      Left exception ->
        hPutStrLn stderr ("Godot.Api: exception in a Haskell callable: " <> show (exception :: SomeException))
    poke callError (GDExtensionCallError GDEXTENSION_CALL_OK 0 0)
{-# NOINLINE onCall #-}

{- Releasing the function. Godot calls this when it destroys the callable,
which can happen inside a Haskell finalizer (a collected 'Callable', or an
array or variant holding one), where calling back into Haskell is not
allowed. So it is the runtime's own C function, not a Haskell one.
-}
onFree :: GDExtensionCallableCustomFree
onFree = GDExtensionCallableCustomFree (castFunPtr hsFreeStablePtr)

foreign import ccall "&hs_free_stable_ptr"
  hsFreeStablePtr :: FunPtr (Ptr () -> IO ())
