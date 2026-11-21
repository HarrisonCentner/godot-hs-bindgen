{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | The hand-written support code that the generated engine API is built on:
the loaded interface, method handles, objects, opaque builtin values and
the layout helpers for a call.

The semantics of calls (how arguments, objects and return buffers are
encoded) follow DavidEichmann/godot-hs (MIT).
-}
module Godot.Api.Runtime (
  -- * Initialization
  initialize,
  functions,
  library,

  -- * Calls
  Ptrcall,
  ptrcallSafe,
  ptrcallUnsafe,
  methodBind,
  allocaFrame,
  pokeAt,
  peekAt,
  setArg,
  fromBool,
  toBool,

  -- * Objects
  GodotClass (..),
  Inherits,
  Handle (..),
  ObjectId (..),
  Ref,
  ObjectFreed (..),
  withObjectPtr,
  adopt,
  maybeAdopt,
  nullObject,
  isAlive,
  upcast,
  convertHandle,
  cast,
  construct,
  singleton,
  freeObject,
  releasePending,
  releaseAtShutdown,

  -- * Opaque builtin values
  Opaque (..),
  newDefault,
  newOpaque,
  newOpaqueWith,
  opaquePtr,
  touch,
  lookupDestructor,
  lookupDefaultConstructor,
  Variant (..),
  withStringName,

  -- * Variants
  Sigma (..),
  variantTypeNumber,
  nilVariant,
  variantFromStorable,
  variantFromOpaque,
  variantFromObject,
  variantToStorable,
  variantToOpaque,
  variantToObject,
  varcall,
  copyVariant,
  assignVariant,

  -- * Builtin methods
  BuiltinMethod,
  lookupBuiltinMethod,
  destroyGString,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, throwIO)
import Control.Monad (replicateM_, unless, when)
import Data.Array (Array, listArray, (!))
import Data.Coerce (Coercible, coerce)
import Data.Functor (void)
import Data.IORef
import Data.Int (Int32, Int64)
import Data.Kind (Type)
import Data.Void (Void)
import Data.Word (Word64, Word8)
import Foreign hiding (fromBool, toBool, void)
import Foreign.C (CChar, CUInt)
import GHC.Foreign qualified as GHC
import GHC.ForeignPtr (unsafeForeignPtrToPtr)
import GHC.IO.Encoding (utf8)
import Godot.GDExtension.Functions (Functions (..), loadFunctions)
import Godot.GDExtension.Interface
import HsBindgen.Runtime.PtrConst qualified as PtrConst
import HsBindgen.Runtime.Support.FunPtr (fromFunPtr, toFunPtr)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem (performMajorGC)

--------------------------------------------------------------------------------
-- Initialization

-- | @object_method_bind_ptrcall(method, object, args, ret)@.
type Ptrcall = Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO ()

data Api = Api
  { apiFunctions :: Functions
  , apiPtrcall :: FunPtr Ptrcall
  , apiVariantDestroy :: FunPtr (Ptr () -> IO ())
  , apiInstanceFromId :: FunPtr (Word64 -> IO (Ptr ()))
  , apiInstanceId :: FunPtr (Ptr () -> IO Word64)
  , apiLibrary :: GDExtensionClassLibraryPtr
  }

apiRef :: IORef (Maybe Api)
apiRef = unsafePerformIO (newIORef Nothing)
{-# NOINLINE apiRef #-}

{- | Load the GDExtension interface. Call this once from the extension's
entry point, with the two arguments Godot passes it, before using anything
else in @Godot.Api@.

Engine classes only exist once Godot reaches the matching initialization
level: scene classes such as @Node@ from the scene level on.

This also asks Godot to call 'releasePending' on its main thread every
frame, which is where reference-counted objects are given back.
-}
initialize :: GDExtensionInterfaceGetProcAddress -> GDExtensionClassLibraryPtr -> IO ()
initialize getProcAddress library' = do
  fns <- loadFunctions getProcAddress
  let getProc =
        unwrapGDExtensionInterfaceGetProcAddress_Aux
          (fromFunPtr (unwrapGDExtensionInterfaceGetProcAddress getProcAddress))
      raw name = do
        GDExtensionInterfaceFunctionPtr fp <-
          GHC.withCString utf8 name (getProc . PtrConst.unsafeFromPtr)
        when (fp == nullFunPtr) $ fail ("Godot.Api: engine does not provide " <> name)
        pure fp
  ptrcall <- raw "object_method_bind_ptrcall"
  variantDestroy <- raw "variant_destroy"
  instanceFromId <- raw "object_get_instance_from_id"
  instanceId <- raw "object_get_instance_id"
  writeIORef apiRef . Just $
    Api
      { apiFunctions = fns
      , apiPtrcall = castFunPtr ptrcall
      , apiVariantDestroy = castFunPtr variantDestroy
      , apiInstanceFromId = castFunPtr instanceFromId
      , apiInstanceId = castFunPtr instanceId
      , apiLibrary = library'
      }
  -- These callbacks live as long as the library, so they are never freed.
  onStartup <- toFunPtr (GDExtensionMainLoopStartupCallback_Aux (pure ()))
  onShutdown <- toFunPtr (GDExtensionMainLoopShutdownCallback_Aux releaseAtShutdown)
  onFrame <- toFunPtr (GDExtensionMainLoopFrameCallback_Aux releasePending)
  with
    GDExtensionMainLoopCallbacks
      { gDExtensionMainLoopCallbacks_startup_func = GDExtensionMainLoopStartupCallback onStartup
      , gDExtensionMainLoopCallbacks_shutdown_func = GDExtensionMainLoopShutdownCallback onShutdown
      , gDExtensionMainLoopCallbacks_frame_func = GDExtensionMainLoopFrameCallback onFrame
      }
    (registerMainLoopCallbacks fns library' . PtrConst.unsafeFromPtr)

-- Read once, on first use after 'initialize'.
api :: Api
api = unsafePerformIO $ readIORef apiRef >>= maybe (fail "Godot.Api: not initialized") pure
{-# NOINLINE api #-}

-- | The interface functions loaded by 'initialize'.
functions :: Functions
functions = let Api {apiFunctions} = api in apiFunctions
{-# NOINLINE functions #-}

{- | The library pointer Godot passed to the entry point: this extension's
identity, for interface functions that ask for it.
-}
library :: GDExtensionClassLibraryPtr
library = let Api {apiLibrary} = api in apiLibrary
{-# NOINLINE library #-}

foreign import ccall safe "dynamic" mkPtrcallSafe :: FunPtr Ptrcall -> Ptrcall

foreign import ccall unsafe "dynamic" mkPtrcallUnsafe :: FunPtr Ptrcall -> Ptrcall

{- | Call a method. Always valid, including when the engine calls back into
Haskell during the call.
-}
ptrcallSafe :: Ptrcall
ptrcallSafe = let Api {apiPtrcall} = api in mkPtrcallSafe apiPtrcall
{-# NOINLINE ptrcallSafe #-}

{- | Call a method without releasing the Haskell runtime. Faster, but the
call must not re-enter Haskell (no signal handlers, virtual methods or
callbacks implemented in Haskell may run during it).
-}
ptrcallUnsafe :: Ptrcall
ptrcallUnsafe = let Api {apiPtrcall} = api in mkPtrcallUnsafe apiPtrcall
{-# NOINLINE ptrcallUnsafe #-}

--------------------------------------------------------------------------------
-- Names and method handles

stringNameType :: GDExtensionVariantType
stringNameType = GDEXTENSION_VARIANT_TYPE_STRING_NAME

stringNameDestructor :: Ptr () -> IO ()
stringNameDestructor = unsafePerformIO $ do
  d <- lookupDestructor (unwrapGDExtensionVariantType stringNameType)
  pure (mkDestructor d)
{-# NOINLINE stringNameDestructor #-}

foreign import ccall unsafe "dynamic" mkDestructor :: FunPtr (Ptr () -> IO ()) -> Ptr () -> IO ()

-- | Run an action with a temporary @StringName@.
withStringName :: String -> (GDExtensionConstStringNamePtr -> IO a) -> IO a
withStringName s k = allocaBytesAligned 8 8 $ \p -> do
  fillBytes p 0 8
  GHC.withCString utf8 s $ \c ->
    stringNameNewWithUtf8Chars
      functions
      (GDExtensionUninitializedStringNamePtr p)
      (PtrConst.unsafeFromPtr c)
  r <- k (GDExtensionConstStringNamePtr (PtrConst.unsafeFromPtr p))
  stringNameDestructor (castPtr p)
  pure r

{- | The handle of an engine method, looked up once. Meant to be bound at the
top level of a generated module.
-}
methodBind :: String -> String -> Int64 -> Ptr ()
methodBind cls name hash = unsafePerformIO $ do
  GDExtensionMethodBindPtr mb <-
    withStringName cls $ \c -> withStringName name $ \n ->
      classdbGetMethodBind functions c n (GDExtensionInt hash)
  let p = castPtr (PtrConst.unsafeToPtr mb)
  when (p == nullPtr) $
    fail ("Godot.Api: method " <> cls <> "." <> name <> " (hash " <> show hash <> ") not found")
  pure p
{-# NOINLINE methodBind #-}

--------------------------------------------------------------------------------
-- Call frames
--
-- Every call uses one temporary block of memory: the array of argument
-- pointers first, then the storage those pointers refer to, then the return
-- value. The generated code knows each offset statically.

allocaFrame :: Int -> (Ptr () -> IO a) -> IO a
allocaFrame n = allocaBytesAligned n 8
{-# INLINE allocaFrame #-}

pokeAt :: (Storable a) => Ptr () -> Int -> a -> IO ()
pokeAt = pokeByteOff
{-# INLINE pokeAt #-}

peekAt :: (Storable a) => Ptr () -> Int -> IO a
peekAt = peekByteOff
{-# INLINE peekAt #-}

-- | Point argument @i@ at the given encoded value.
setArg :: Ptr () -> Int -> Ptr () -> IO ()
setArg frame i = pokeByteOff frame (i * 8)
{-# INLINE setArg #-}

fromBool :: Bool -> Word8
fromBool b = if b then 1 else 0
{-# INLINE fromBool #-}

toBool :: Word8 -> Bool
toBool = (/= 0)
{-# INLINE toBool #-}

--------------------------------------------------------------------------------
-- Objects
--
-- Godot has two kinds of object, and a handle for each:
--

-- * Most objects (nodes, singletons) live until something frees them. A

--   handle to one is its instance ID, which Godot can check: using a handle
--   after its object is gone throws 'ObjectFreed' instead of touching freed
--   memory.
--

-- * Objects of classes that inherit @RefCounted@ (resources such as meshes

--   and materials) live while anything holds a reference. A handle to one
--   owns a reference, so the object cannot go away while the handle is
--   reachable, and gives it back when the handle is garbage collected.

{- | Godot's identifier for one object. It is not reused while the object is
alive, and Godot can tell whether it still refers to anything. Zero is no
object.
-}
newtype ObjectId = ObjectId Word64
  deriving newtype (Eq, Ord, Show)

-- | An owned reference to a reference-counted object.
data Ref = Ref (ForeignPtr ()) ObjectId

instance Eq Ref where
  Ref _ a == Ref _ b = a == b

instance Ord Ref where
  compare (Ref _ a) (Ref _ b) = compare a b

instance Show Ref where
  showsPrec d (Ref _ i) = showsPrec d i

-- | What a handle is underneath.
data Handle
  = -- | An object freed by hand, known by its instance ID.
    Managed ObjectId
  | -- | A reference-counted object, with a reference this handle owns.
    Counted Ref

-- | A Godot engine class. Values are handles to a Godot @Object@.
class GodotClass a where
  godotClassName :: String

  -- | Whether the class inherits @RefCounted@.
  isRefCounted :: Bool

  toHandle :: a -> Handle
  fromHandle :: Handle -> a

-- | @Inherits a b@: class @a@ is @b@ or one of its subclasses.
class (GodotClass a, GodotClass b) => Inherits a b

-- | A handle was used after its object was freed; names the handle's class.
newtype ObjectFreed = ObjectFreed String
  deriving stock (Show)

instance Exception ObjectFreed

foreign import ccall unsafe "dynamic"
  mkInstanceFromId :: FunPtr (Word64 -> IO (Ptr ())) -> Word64 -> IO (Ptr ())

foreign import ccall unsafe "dynamic"
  mkInstanceId :: FunPtr (Ptr () -> IO Word64) -> Ptr () -> IO Word64

-- | The object with this instance ID, or null if there is none any more.
instanceFromId :: Word64 -> IO (Ptr ())
instanceFromId = let Api {apiInstanceFromId} = api in mkInstanceFromId apiInstanceFromId
{-# NOINLINE instanceFromId #-}

instanceId :: Ptr () -> IO Word64
instanceId = let Api {apiInstanceId} = api in mkInstanceId apiInstanceId
{-# NOINLINE instanceId #-}

{- | Use the object's pointer for the duration of the action. Throws
'ObjectFreed' if the object no longer exists. The null handle gives a null
pointer.
-}
withObjectPtr :: forall a r. (GodotClass a) => a -> (Ptr () -> IO r) -> IO r
withObjectPtr a k = case toHandle a of
  Managed (ObjectId 0) -> k nullPtr
  Managed (ObjectId i) -> do
    p <- instanceFromId i
    when (p == nullPtr) (throwIO (ObjectFreed (godotClassName @a)))
    k p
  Counted (Ref fp _) -> withForeignPtr fp k
{-# INLINE withObjectPtr #-}

{- | A handle for a non-null object pointer the engine handed over. For a
reference-counted class the pointer must come with a reference, which the
handle takes ownership of.
-}
adopt :: forall a. (GodotClass a) => Ptr () -> IO a
adopt p
  | p == nullPtr = fail ("Godot.Api: null " <> godotClassName @a <> " where Godot declares there is one")
  | isRefCounted @a = fromHandle . Counted <$> adoptRef p
  | otherwise = fromHandle . Managed . ObjectId <$> instanceId p
{-# INLINE adopt #-}

-- | 'adopt', with 'Nothing' for a null pointer.
maybeAdopt :: forall a. (GodotClass a) => Ptr () -> IO (Maybe a)
maybeAdopt p = if p == nullPtr then pure Nothing else Just <$> adopt p
{-# INLINE maybeAdopt #-}

foreign import ccall "&godot_hs_release_later"
  releaseLater :: FunPtr (Ptr () -> IO ())

foreign import ccall unsafe "godot_hs_release_pop"
  releasePop :: IO (Ptr ())

-- Take ownership of one reference to the object.
adoptRef :: Ptr () -> IO Ref
adoptRef p = do
  i <- instanceId p
  fp <- newForeignPtr releaseLater p
  pure (Ref fp (ObjectId i))

noRef :: Ref
noRef = Ref (unsafePerformIO (newForeignPtr_ nullPtr)) (ObjectId 0)
{-# NOINLINE noRef #-}

-- | The null object, for arguments that accept none.
nullObject :: forall a. (GodotClass a) => a
nullObject
  | isRefCounted @a = fromHandle (Counted noRef)
  | otherwise = fromHandle (Managed (ObjectId 0))

{- | Whether the handle's object still exists. Always true for a
reference-counted object, which the handle keeps alive.
-}
isAlive :: (GodotClass a) => a -> IO Bool
isAlive a = case toHandle a of
  Managed (ObjectId 0) -> pure False
  Managed (ObjectId i) -> (/= nullPtr) <$> instanceFromId i
  Counted (Ref _ (ObjectId i)) -> pure (i /= 0)

-- | Treat an object as one of its base classes.
upcast :: (Inherits a b) => a -> b
upcast = convertHandle
{-# INLINE upcast #-}

{- | The same object as a handle of another class, without checking that the
object is of that class. A reference-counted handle keeps its reference when
the target class is reference-counted too; converted to a class that is not
(only @Object@), it becomes an instance ID that does not keep the object
alive.
-}
convertHandle :: forall a b. (GodotClass a, GodotClass b) => a -> b
convertHandle a = fromHandle $ case toHandle a of
  Counted ref@(Ref _ i) -> if isRefCounted @b then Counted ref else Managed i
  Managed i -> Managed i
{-# INLINE convertHandle #-}

-- | Checked conversion to another class.
cast :: forall b a. (GodotClass a, GodotClass b) => a -> IO (Maybe b)
cast a = withObjectPtr a $ \p ->
  if p == nullPtr
    then pure Nothing
    else do
      tag <- withStringName (godotClassName @b) (classdbGetClassTag functions)
      GDExtensionObjectPtr r <-
        objectCastTo functions (GDExtensionConstObjectPtr (PtrConst.unsafeFromPtr (castPtr p))) tag
      if r == nullPtr
        then pure Nothing
        else
          Just <$> case toHandle a of
            Counted ref | isRefCounted @b -> pure (fromHandle (Counted ref))
            _
              | isRefCounted @b -> do
                  -- From a handle that owns no reference: take one.
                  referenced <- callBool refCountedReference (castPtr r)
                  unless referenced $
                    fail ("Godot.Api: " <> godotClassName @b <> " is being destroyed")
                  fromHandle . Counted <$> adoptRef (castPtr r)
              | otherwise -> fromHandle . Managed . ObjectId <$> instanceId (castPtr r)

-- The hashes below are checked against the API by the generator.

objectNotification :: Ptr ()
objectNotification = methodBind "Object" "notification" 4023243586
{-# NOINLINE objectNotification #-}

refCountedInitRef, refCountedReference, refCountedUnreference :: Ptr ()
refCountedInitRef = methodBind "RefCounted" "init_ref" 2240911060
refCountedReference = methodBind "RefCounted" "reference" 2240911060
refCountedUnreference = methodBind "RefCounted" "unreference" 2240911060
{-# NOINLINE refCountedInitRef #-}
{-# NOINLINE refCountedReference #-}
{-# NOINLINE refCountedUnreference #-}

-- Call a method that takes no arguments and returns a bool.
callBool :: Ptr () -> Ptr () -> IO Bool
callBool bind object = allocaFrame 8 $ \fr -> do
  pokeAt fr 0 (0 :: Word8)
  ptrcallSafe bind object fr fr
  toBool <$> peekAt fr 0

-- | Create a new instance of a class.
construct :: forall a. (GodotClass a) => IO a
construct = do
  GDExtensionObjectPtr created <- withStringName (godotClassName @a) (classdbConstructObject2 functions)
  let p = castPtr created
  when (p == nullPtr) $ fail ("Godot.Api: cannot construct " <> godotClassName @a)
  -- classdb_construct_object2 leaves NOTIFICATION_POSTINITIALIZE (0) to us.
  allocaFrame 32 $ \fr -> do
    pokeAt fr 16 (0 :: Int64)
    pokeAt fr 24 (0 :: Word8)
    setArg fr 0 (fr `plusPtr` 16)
    setArg fr 1 (fr `plusPtr` 24)
    ptrcallSafe objectNotification p fr nullPtr
  -- A new reference-counted object holds one reference nobody owns yet;
  -- claim it for the handle.
  when (isRefCounted @a) $ void (callBool refCountedInitRef p)
  adopt p

-- | The engine singleton of a class.
singleton :: forall a. (GodotClass a) => IO a
singleton = do
  GDExtensionObjectPtr p <- withStringName (godotClassName @a) (globalGetSingleton functions)
  when (p == nullPtr) $ fail ("Godot.Api: no singleton " <> godotClassName @a)
  adopt (castPtr p)

{- | Destroy an object. Not for reference-counted objects, which are released
when their handles are collected.
-}
freeObject :: forall a. (GodotClass a) => a -> IO ()
freeObject a
  | isRefCounted @a = fail ("Godot.Api: " <> godotClassName @a <> " is reference-counted and cannot be freed by hand")
  | otherwise = withObjectPtr a $ \p ->
      unless (p == nullPtr) (objectDestroy functions (GDExtensionObjectPtr (castPtr p)))

{- | Give back the references of reference-counted objects whose handles
have been garbage collected, destroying the objects nothing else holds.

Must run on Godot's main thread. 'initialize' arranges for Godot to call it
every frame, so calling it yourself is only needed where no frames run, as in
tests.
-}
releasePending :: IO ()
releasePending = do
  p <- releasePop
  unless (p == nullPtr) $ do
    unreferenced <- callBool refCountedUnreference p
    when unreferenced (objectDestroy functions (GDExtensionObjectPtr (castPtr p)))
    releasePending

{- | Collect garbage and release what that frees, a few times over: for
when the engine is about to shut down, so that objects only unreachable
handles were keeping alive do not outlive it.

The runtime runs finalizers shortly after a collection, not during it, hence
the short pauses. Handles that are still reachable keep their objects, which
Godot then reports as leaked.
-}
releaseAtShutdown :: IO ()
releaseAtShutdown = replicateM_ 3 $ do
  performMajorGC
  threadDelay 5000
  releasePending

--------------------------------------------------------------------------------
-- Opaque builtin values

{- | A builtin type that Godot owns the representation of (@String@, @Array@,
...). The value lives in memory managed by the Haskell garbage collector
and is destroyed with Godot's destructor when collected.
-}
class (Coercible a (ForeignPtr ())) => Opaque a where
  opaqueSize :: Int
  opaqueDestructor :: FunPtr (Ptr () -> IO ())

  -- | Initialize uninitialized memory to the type's default value.
  opaqueDefault :: Ptr () -> IO ()

{- | The type's default value: an empty string, an empty array, a nil
variant and so on.
-}
newDefault :: forall a. (Opaque a) => IO a
newDefault = newOpaque @a (\_ -> pure ())

{- | A new default-constructed value, then modified in place. Used for
results, which Godot assigns into.
-}
newOpaque :: forall a. (Opaque a) => (Ptr () -> IO ()) -> IO a
newOpaque k = newOpaqueWith @a (\p -> opaqueDefault @a p >> k p)

{- | A new value initialized by the given function, which receives
uninitialized (zeroed) memory.
-}
newOpaqueWith :: forall a. (Opaque a) => (Ptr () -> IO ()) -> IO a
newOpaqueWith initFn = do
  fp <- mallocForeignPtrBytes (opaqueSize @a)
  withForeignPtr fp $ \p -> do
    fillBytes p 0 (opaqueSize @a)
    initFn p
  when (opaqueDestructor @a /= nullFunPtr) $
    addForeignPtrFinalizer (opaqueDestructor @a) fp
  pure (coerce fp)

-- | Only valid while the value is kept alive; follow the use with 'touch'.
opaquePtr :: forall a. (Opaque a) => a -> Ptr ()
opaquePtr a = unsafeForeignPtrToPtr (coerce a :: ForeignPtr ())
{-# INLINE opaquePtr #-}

touch :: forall a. (Opaque a) => a -> IO ()
touch a = touchForeignPtr (coerce a :: ForeignPtr ())
{-# INLINE touch #-}

-- | Godot's destructor for a variant type, by its number.
lookupDestructor :: CUInt -> IO (FunPtr (Ptr () -> IO ()))
lookupDestructor vt = do
  GDExtensionPtrDestructor d <- variantGetPtrDestructor functions (GDExtensionVariantType vt)
  pure (castFunPtr d)

foreign import ccall unsafe "dynamic"
  mkConstructor :: FunPtr (Ptr () -> Ptr () -> IO ()) -> Ptr () -> Ptr () -> IO ()

-- | Godot's argument-less constructor for a variant type, by its number.
lookupDefaultConstructor :: CUInt -> IO (Ptr () -> IO ())
lookupDefaultConstructor vt = do
  GDExtensionPtrConstructor c <- variantGetPtrConstructor functions (GDExtensionVariantType vt) 0
  when (c == nullFunPtr) $ fail ("Godot.Api: no default constructor for variant type " <> show vt)
  pure (\p -> mkConstructor (castFunPtr c) p nullPtr)

-- | A Godot @Variant@: a value of any type.
newtype Variant = Variant (ForeignPtr ())

instance Opaque Variant where
  opaqueSize = 24
  opaqueDestructor = let Api {apiVariantDestroy} = api in apiVariantDestroy
  opaqueDefault p = variantNewNil functions (GDExtensionUninitializedVariantPtr (castPtr p))

--------------------------------------------------------------------------------
-- Variants

{- | A value together with a witness of its type: the dependent pair from
Justin Le's "Seven Levels of Type Safety in Haskell". With the closed
witness @VType@ of "Godot.Api.Variant" this is what a 'Variant' holds.
-}
data Sigma :: (Type -> Type) -> Type where
  MkSigma :: p a -> a -> Sigma p

-- | The number of the variant type a variant currently holds.
variantTypeNumber :: Variant -> IO CUInt
variantTypeNumber v = do
  GDExtensionVariantType n <-
    variantGetType functions (GDExtensionConstVariantPtr (PtrConst.unsafeFromPtr (castPtr (opaquePtr v))))
  touch v
  pure n

{- | Godot's converters between a variant and a value of one variant type.
Both take the destination first, as uninitialized memory.
-}
data Converters = Converters
  { intoVariant :: Ptr () -> Ptr () -> IO ()
  , outOfVariant :: Ptr () -> Ptr () -> IO ()
  }

foreign import ccall unsafe "dynamic"
  mkConverter :: FunPtr (Ptr () -> Ptr () -> IO ()) -> Ptr () -> Ptr () -> IO ()

-- One entry per variant type, each looked up the first time it is used.
-- Type 0 is nil, which has no converters.
converters :: Array Int Converters
converters = listArray (1, 38) [unsafePerformIO (load n) | n <- [1 .. 38]]
  where
    load n = do
      let vt = GDExtensionVariantType n
      GDExtensionVariantFromTypeConstructorFunc into <- getVariantFromTypeConstructor functions vt
      GDExtensionTypeFromVariantConstructorFunc out <- getVariantToTypeConstructor functions vt
      pure Converters {intoVariant = mkConverter (castFunPtr into), outOfVariant = mkConverter (castFunPtr out)}
{-# NOINLINE converters #-}

convertersFor :: CUInt -> Converters
convertersFor n = converters ! fromIntegral n

nilVariant :: IO Variant
nilVariant = newOpaque (\_ -> pure ())

-- | A variant of the numbered type from a value with a fixed layout.
variantFromStorable :: (Storable a) => CUInt -> a -> IO Variant
variantFromStorable n a =
  let Converters {intoVariant} = convertersFor n
   in with a $ \p -> newOpaqueWith (\v -> intoVariant v (castPtr p))

variantFromOpaque :: (Opaque a) => CUInt -> a -> IO Variant
variantFromOpaque n a = do
  let Converters {intoVariant} = convertersFor n
  v <- newOpaqueWith (\v -> intoVariant v (opaquePtr a))
  touch a
  pure v

-- | A variant holding an object, or no object.
variantFromObject :: (GodotClass a) => CUInt -> Maybe a -> IO Variant
variantFromObject n = \case
  Nothing -> variantFromStorable n (nullPtr :: Ptr ())
  Just object -> withObjectPtr object (variantFromStorable n)

{- | Read a variant as the numbered type. Unchecked: the variant must hold
that type.
-}
variantToStorable :: (Storable a) => CUInt -> Variant -> IO a
variantToStorable n v = do
  let Converters {outOfVariant} = convertersFor n
  r <- alloca $ \p -> outOfVariant (castPtr p) (opaquePtr v) >> peek p
  touch v
  pure r

variantToOpaque :: (Opaque a) => CUInt -> Variant -> IO a
variantToOpaque n v = do
  let Converters {outOfVariant} = convertersFor n
  r <- newOpaqueWith (\p -> outOfVariant p (opaquePtr v))
  touch v
  pure r

{- | The object a variant holds, if any. The variant keeps its own
reference, so this is only for classes that are not reference-counted.
-}
variantToObject :: (GodotClass a) => CUInt -> Variant -> IO (Maybe a)
variantToObject n v = variantToStorable n v >>= maybeAdopt

{- | Call a method through Godot's dynamic path, with every argument a
variant: for methods that take a variable number of arguments. Throws if
Godot rejects the call (wrong argument count or type, no such method).
-}
varcall :: Ptr () -> Ptr () -> [Variant] -> IO Variant
varcall bind object args =
  withArrayLen (map opaquePtr args) $ \argc argv ->
    alloca $ \callError -> do
      poke callError (GDExtensionCallError GDEXTENSION_CALL_OK 0 0)
      result <- newOpaque $ \ret ->
        objectMethodBindCall
          functions
          (GDExtensionMethodBindPtr (PtrConst.unsafeFromPtr (castPtr bind)))
          (GDExtensionObjectPtr (castPtr object))
          (PtrConst.unsafeFromPtr (castPtr argv))
          (GDExtensionInt (fromIntegral argc))
          (GDExtensionUninitializedVariantPtr (castPtr ret))
          callError
      mapM_ touch args
      GDExtensionCallError {gDExtensionCallError_error, gDExtensionCallError_argument, gDExtensionCallError_expected} <-
        peek callError
      unless (gDExtensionCallError_error == GDEXTENSION_CALL_OK) $
        throwIO . userError $
          "Godot.Api: call failed with error "
            <> show (unwrapGDExtensionCallErrorType gDExtensionCallError_error)
            <> " (argument "
            <> show gDExtensionCallError_argument
            <> ", expected "
            <> show gDExtensionCallError_expected
            <> ")"
      pure result

{- | A variant we own, copied from one the engine owns at the given
address.
-}
copyVariant :: Ptr () -> IO Variant
copyVariant source =
  newOpaqueWith $ \target ->
    variantNewCopy
      functions
      (GDExtensionUninitializedVariantPtr (castPtr target))
      (GDExtensionConstVariantPtr (PtrConst.unsafeFromPtr (castPtr source)))

-- | Overwrite the variant at an address, which must already hold one.
assignVariant :: Ptr () -> Variant -> IO ()
assignVariant target value = do
  variantDestroy functions (GDExtensionVariantPtr (castPtr target))
  variantNewCopy
    functions
    (GDExtensionUninitializedVariantPtr (castPtr target))
    (GDExtensionConstVariantPtr (PtrConst.unsafeFromPtr (castPtr (opaquePtr value))))
  touch value

--------------------------------------------------------------------------------
-- Builtin methods

{- | A method of a builtin type: self, the argument pointers, where to put
the result, and the argument count.
-}
type BuiltinMethod = Ptr () -> Ptr () -> Ptr () -> Int32 -> IO ()

foreign import ccall unsafe "dynamic"
  mkBuiltinMethod :: FunPtr BuiltinMethod -> BuiltinMethod

-- | Look up a method of the builtin type with the given variant type number.
lookupBuiltinMethod :: CUInt -> String -> Int64 -> IO BuiltinMethod
lookupBuiltinMethod vt name hash = do
  GDExtensionPtrBuiltInMethod method <-
    withStringName name $ \n ->
      variantGetPtrBuiltinMethod functions (GDExtensionVariantType vt) n (GDExtensionInt hash)
  when (method == nullFunPtr) $
    fail ("Godot.Api: builtin method " <> name <> " of variant type " <> show vt <> " not found")
  pure (mkBuiltinMethod (castFunPtr method))

-- | Destroy the Godot @String@ at an address, leaving it uninitialized.
destroyGString :: Ptr () -> IO ()
destroyGString = unsafePerformIO (mkDestructor <$> lookupDestructor 4)
{-# NOINLINE destroyGString #-}

-- Silence unused-import warnings for types only used in signatures above.
_unused :: (Ptr Void, Ptr CChar)
_unused = (nullPtr, nullPtr)
