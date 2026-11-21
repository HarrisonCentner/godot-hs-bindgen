{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The test suite. The engine API can only be exercised with an engine
running, so the tests are an extension: Godot loads this library, and the
hedgehog properties run once the scene classes exist.
-}
module Tests.Entry (hs_tests_init) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, replicateM, replicateM_, unless, void, when)
import Data.Either (isLeft)
import Data.Function (on)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Int (Int32, Int64)
import Data.List (nubBy)
import Data.Maybe (isJust)
import Data.Type.Equality (testEquality)
import Data.Void (Void)
import Data.Word (Word8)
import Foreign hiding (void)
import Godot.Api
import Godot.Api.Class.BoxMesh qualified as BoxMesh
import Godot.Api.Class.MeshInstance3D qualified as MeshInstance3D
import Godot.Api.Class.Node qualified as Node
import Godot.Api.Class.Node3D qualified as Node3D
import Godot.Api.Class.OS qualified as OS
import Godot.Api.Class.Object qualified as Object
import Godot.Api.Class.RefCounted qualified as RefCounted
import Godot.GDExtension.Interface
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import HsBindgen.Runtime.Support.FunPtr (toFunPtr)
import System.IO
import System.Mem (performMajorGC)

hs_tests_init ::
  GDExtensionInterfaceGetProcAddress ->
  GDExtensionClassLibraryPtr ->
  Ptr GDExtensionInitialization ->
  IO GDExtensionBool
hs_tests_init getProcAddress library rInit = do
  hSetBuffering stdout LineBuffering
  initialize getProcAddress library
  onInitialize <- toFunPtr (GDExtensionInitializeCallback_Aux onLevel)
  onDeinitialize <- toFunPtr (GDExtensionDeinitializeCallback_Aux (\_ _ -> pure ()))
  poke
    rInit
    GDExtensionInitialization
      { gDExtensionInitialization_minimum_initialization_level =
          GDEXTENSION_INITIALIZATION_SCENE
      , gDExtensionInitialization_userdata = nullPtr
      , gDExtensionInitialization_initialize =
          GDExtensionInitializeCallback onInitialize
      , gDExtensionInitialization_deinitialize =
          GDExtensionDeinitializeCallback onDeinitialize
      }
  pure (GDExtensionBool 1)

onLevel :: Ptr Void -> GDExtensionInitializationLevel -> IO ()
onLevel _ level = when (level == GDEXTENSION_INITIALIZATION_SCENE) $ do
  -- Hedgehog's report is not ASCII, and the locale may be plain C (it is in
  -- a nix build), where writing it would throw.
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  -- An exception must not escape into the engine, which called this.
  outcome <- try (traverse checkSequential [variants, calls, collections, lifetimes, memory])
  -- The flake check looks for this line.
  putStrLn $ case outcome of
    Right results | and results -> "[hs-tests] PASSED"
    Right _ -> "[hs-tests] FAILED"
    Left e -> "[hs-tests] FAILED: " <> show (e :: SomeException)

--------------------------------------------------------------------------------
-- Variants

variants :: Group
variants =
  Group
    "Godot.Api.Variant"
    [ ("Bool round-trips", roundTrips Gen.bool)
    , ("Int64 round-trips", roundTrips (Gen.int64 Range.linearBounded))
    , ("Double round-trips", roundTrips (Gen.double (Range.linearFracFrom 0 (-1e12) 1e12)))
    , ("Vector3 round-trips", roundTrips genVector3)
    , ("Color round-trips", roundTrips (Color <$> genFloat <*> genFloat <*> genFloat <*> genFloat))
    , ("Transform3D round-trips", roundTrips genTransform3D)
    , ("nil round-trips", withTests 1 (roundTrips (pure ())))
    ,
      ( "String round-trips through GString"
      , property $ do
          s <- forAll genText
          back <- evalIO (toGString s >>= toVariant >>= fromVariant >>= traverse fromGString)
          back === Just s
      )
    ,
      ( "String round-trips through StringName"
      , property $ do
          s <- forAll genText
          back <- evalIO (toStringName s >>= toVariant >>= fromVariant >>= traverse fromStringName)
          back === Just s
      )
    ,
      ( "viewVariant witnesses the stored type"
      , property $ do
          n <- forAll (Gen.int64 Range.linearBounded)
          MkSigma witness _ <- evalIO (toVariant n >>= viewVariant)
          assert (isJust (testEquality witness VInt))
      )
    ,
      ( "viewVariant returns the stored value"
      , property $ do
          v <- forAll genVector3
          view <- evalIO (toVariant v >>= viewVariant)
          case view of
            MkSigma VVector3 back -> back === v
            MkSigma other _ -> annotateShow other >> failure
      )
    ,
      ( "fromVariant does not coerce between types"
      , withTests 1 . property $ do
          asDouble <- evalIO (toVariant (5 :: Int64) >>= fromVariant @Double)
          asDouble === Nothing
      )
    ,
      ( "an object variant holds the same object"
      , withTests 1 . property $ do
          back <- evalIO $ do
            node <- Node3D.construct
            let object = upcast node :: Object
            r <- toVariant (Just object) >>= fromVariant
            freeObject node
            pure (r == Just (Just object))
          assert back
      )
    ,
      ( "an object variant may hold no object"
      , withTests 1 . property $ do
          back <- evalIO (toVariant (Nothing :: Maybe Object) >>= fromVariant)
          back === Just (Nothing :: Maybe Object)
      )
    ]

roundTrips :: (VariantValue a, Eq a, Show a) => Gen a -> Property
roundTrips gen = property $ do
  x <- forAll gen
  back <- evalIO (toVariant x >>= fromVariant)
  back === Just x

genFloat :: Gen Float
genFloat = Gen.float (Range.linearFracFrom 0 (-1e6) 1e6)

genVector3 :: Gen Vector3
genVector3 = Vector3 <$> genFloat <*> genFloat <*> genFloat

genTransform3D :: Gen Transform3D
genTransform3D =
  Transform3D <$> (Basis <$> genVector3 <*> genVector3 <*> genVector3) <*> genVector3

-- | Text without NUL, which ends a string on the Godot side.
genText :: Gen String
genText = Gen.string (Range.linear 0 40) (Gen.filter (/= '\0') Gen.unicode)

--------------------------------------------------------------------------------
-- Methods taking a variable number of arguments

calls :: Group
calls =
  Group
    "variable-argument methods"
    [
      ( "Object.call returns the method's result as a variant"
      , property $ do
          name <- forAll (Gen.string (Range.linear 1 20) Gen.alphaNum)
          back <- evalIO $ do
            node <- Node3D.construct
            Node.setName node =<< toStringName name
            getName <- toStringName "get_name"
            result <- Object.call node getName [] >>= viewVariant
            freeObject node
            case result of
              MkSigma VStringName n -> Just <$> fromStringName n
              _ -> pure Nothing
          back === Just name
      )
    ,
      ( "Object.call passes arguments"
      , withTests 1 . property $ do
          back <- evalIO $ do
            node <- Node3D.construct
            setPosition <- toStringName "set_position"
            argument <- toVariant (Vector3 4 5 6)
            _ <- Object.call node setPosition [argument]
            position <- Node3D.getPosition node
            freeObject node
            pure position
          back === Vector3 4 5 6
      )
    ,
      ( "Object.call throws when the method does not exist"
      , withTests 1 . property $ do
          result <- evalIO $ do
            node <- Node3D.construct
            missing <- toStringName "no_such_method"
            r <- try (Object.call node missing []) :: IO (Either SomeException Variant)
            freeObject node
            pure (isLeft r)
          assert result
      )
    ,
      ( "Object.emitSignal emits a user signal"
      , withTests 1 . property $ do
          result <- evalIO $ do
            node <- Node3D.construct
            noArguments <- newDefault
            Object.addUserSignal node <$> toGString "pulse" <*> pure noArguments >>= id
            signal <- toStringName "pulse"
            argument <- toVariant (1 :: Int64)
            r <- Object.emitSignal node signal [argument]
            freeObject node
            pure r
          result === OK
      )
    ]

--------------------------------------------------------------------------------
-- Collections and callables

collections :: Group
collections =
  Group
    "collections and callables"
    [
      ( "a list round-trips through an Array"
      , property $ do
          xs <- forAll (Gen.list (Range.linear 0 50) (Gen.int64 Range.linearBounded))
          back <- evalIO (arrayFromList xs >>= arrayToList)
          back === Just xs
      )
    ,
      ( "an Array read at the wrong element type gives Nothing"
      , withTests 1 . property $ do
          back <- evalIO $ do
            mixed <- sequence [toVariant (1 :: Int64), toVariant True]
            arrayFromVariants mixed >>= arrayToList @Int64
          back === Nothing
      )
    ,
      ( "entries round-trip through a Dictionary, in order"
      , property $ do
          entries <-
            forAll . fmap (nubBy ((==) `on` fst)) $
              Gen.list (Range.linear 0 30) ((,) <$> Gen.int64 Range.linearBounded <*> genVector3)
          back <- evalIO (dictionaryFromList entries >>= dictionaryToList)
          back === Just entries
      )
    ,
      ( "a later Dictionary entry replaces an earlier one with the same key"
      , withTests 1 . property $ do
          back <- evalIO (dictionaryFromList [(1 :: Int64, 10 :: Int64), (2, 5), (1, 20)] >>= dictionaryToList)
          back === Just [(1 :: Int64, 20 :: Int64), (2, 5)]
      )
    , ("a list round-trips through a PackedByteArray", packedRoundTrips @PackedByteArray (Gen.word8 Range.linearBounded))
    , ("a list round-trips through a PackedInt32Array", packedRoundTrips @PackedInt32Array (Gen.int32 Range.linearBounded))
    , ("a list round-trips through a PackedFloat32Array", packedRoundTrips @PackedFloat32Array genFloat)
    , ("a list round-trips through a PackedVector3Array", packedRoundTrips @PackedVector3Array genVector3)
    ,
      ( "a list round-trips through a PackedColorArray"
      , packedRoundTrips @PackedColorArray (Color <$> genFloat <*> genFloat <*> genFloat <*> genFloat)
      )
    ,
      ( "strings round-trip through a PackedStringArray"
      , property $ do
          strings <- forAll (Gen.list (Range.linear 0 20) genText)
          back <- evalIO (packedStringsFromList strings >>= packedStringsToList)
          back === strings
      )
    ,
      ( "a signal runs a connected Haskell callable with its arguments"
      , property $ do
          arguments <- forAll (Gen.list (Range.linear 0 5) (Gen.int64 Range.linearBounded))
          (connected, received) <- evalIO $ do
            calls <- newIORef []
            callable <- newCallable_ $ \values -> do
              numbers <- traverse (fromVariant @Int64) values
              modifyIORef calls (numbers :)
            node <- Node3D.construct
            noArguments <- newDefault
            name <- toGString "pulse"
            Object.addUserSignal node name noArguments
            signal <- toStringName "pulse"
            connected <- Object.connect node signal callable 0
            _ <- Object.emitSignal node signal =<< traverse toVariant arguments
            freeObject node
            (,) connected <$> readIORef calls
          connected === OK
          received === [map Just arguments]
      )
    ]

packedRoundTrips ::
  forall p.
  (Packed p, Eq (Element p), Show (Element p)) =>
  Gen (Element p) ->
  Property
packedRoundTrips gen = property $ do
  xs <- forAll (Gen.list (Range.linear 0 200) gen)
  back <- evalIO (packedFromList @p xs >>= packedToList)
  back === xs

--------------------------------------------------------------------------------
-- Object lifetimes

lifetimes :: Group
lifetimes =
  Group
    "object lifetimes"
    [
      ( "using a freed node throws ObjectFreed"
      , withTests 1 . property $ do
          result <- evalIO $ do
            node <- Node3D.construct
            freeObject node
            try (Node3D.getPosition node)
          case result of
            Left (ObjectFreed cls) -> cls === "Node3D"
            Right position -> annotateShow position >> failure
      )
    ,
      ( "isAlive follows the object"
      , withTests 1 . property $ do
          states <- evalIO $ do
            node <- Node3D.construct
            before <- isAlive node
            freeObject node
            after <- isAlive node
            pure (before, after)
          states === (True, False)
      )
    ,
      ( "a child freed with its parent is no longer alive"
      , withTests 1 . property $ do
          alive <- evalIO $ do
            parent <- Node3D.construct
            child <- Node3D.construct
            Node.addChild parent child False Node_INTERNAL_MODE_DISABLED
            freeObject parent
            isAlive child
          alive === False
      )
    ,
      ( "a new reference-counted object has one reference"
      , withTests 1 . property $ do
          count <- evalIO (BoxMesh.construct >>= RefCounted.getReferenceCount)
          count === 1
      )
    ,
      ( "a handle keeps its object alive after the node using it is freed"
      , withTests 1 . property $ do
          counts <- evalIO $ do
            cube <- MeshInstance3D.construct
            mesh <- BoxMesh.construct
            MeshInstance3D.setMesh cube mesh
            shared <- RefCounted.getReferenceCount mesh
            freeObject cube
            alone <- RefCounted.getReferenceCount mesh
            pure (shared, alone)
          counts === (2, 1)
      )
    ,
      ( "each result handle owns a reference, returned when it is collected"
      , withTests 1 . property $ do
          counts <- evalIO $ do
            cube <- MeshInstance3D.construct
            mesh <- BoxMesh.construct
            MeshInstance3D.setMesh cube mesh
            held <- countWhileHoldingResults cube
            settleUntil ((== 2) <$> RefCounted.getReferenceCount mesh)
            released <- RefCounted.getReferenceCount mesh
            freeObject cube
            pure (held, released)
          -- The node and our handle, plus ten results; then just those two.
          counts === (12, 2)
      )
    ,
      ( "objects are destroyed once their handles are collected"
      , withTests 1 . property $ do
          alive <- evalIO $ do
            objects <- constructAndForget 100
            settleUntil (not . or <$> traverse isAlive objects)
            length . filter id <$> traverse isAlive objects
          alive === 0
      )
    ,
      ( "an object returned by the engine is destroyed once everything lets go"
      , withTests 1 . property $ do
          states <- evalIO $ do
            cube <- MeshInstance3D.construct
            mesh <- attachMeshAndFetchIt cube
            whileUsed <- isAlive mesh
            freeObject cube
            settleUntil (not <$> isAlive mesh)
            afterwards <- isAlive mesh
            pure (whileUsed, afterwards)
          states === (True, False)
      )
    ,
      ( "an upcast and a cast back give the same object"
      , withTests 1 . property $ do
          same <- evalIO $ do
            mesh <- BoxMesh.construct
            back <- cast @BoxMesh (upcast mesh :: Object)
            pure (back == Just mesh)
          assert same
      )
    ,
      ( "casting an object handle to a reference-counted class takes a reference"
      , withTests 1 . property $ do
          count <- evalIO $ do
            mesh <- BoxMesh.construct
            let unowned = upcast mesh :: Object
            owned <- cast @BoxMesh unowned
            n <- RefCounted.getReferenceCount mesh
            -- Keep the second handle alive until the count is read.
            pure (n, owned == Just mesh)
          count === (2, True)
      )
    ,
      ( "a reference-counted object cannot be freed by hand"
      , withTests 1 . property $ do
          result <- evalIO $ do
            mesh <- BoxMesh.construct
            try (freeObject mesh) :: IO (Either SomeException ())
          assert (isLeft result)
      )
    ]

{- | Meshes whose owning handles are unreachable once this returns. What is
returned are instance IDs, which do not keep the objects alive.
-}
constructAndForget :: Int -> IO [Object]
constructAndForget n = replicateM n (upcast <$> BoxMesh.construct)
{-# NOINLINE constructAndForget #-}

{- | Give the cube a new mesh and read it back through the engine, so that
the mesh has been held by a constructed handle and by a result handle.
Returns its instance ID only.
-}
attachMeshAndFetchIt :: MeshInstance3D -> IO Object
attachMeshAndFetchIt cube = do
  MeshInstance3D.setMesh cube =<< BoxMesh.construct
  fetched <- MeshInstance3D.getMesh cube
  maybe (fail "the cube has no mesh") (pure . upcast) fetched
{-# NOINLINE attachMeshAndFetchIt #-}

{- | The reference count of the cube's mesh while ten handles returned by
'MeshInstance3D.getMesh' are alive. They are unreachable once this returns.
-}
countWhileHoldingResults :: MeshInstance3D -> IO Int64
countWhileHoldingResults cube = do
  results <- replicateM 10 (MeshInstance3D.getMesh cube)
  case results of
    Just mesh : _ -> RefCounted.getReferenceCount mesh
    _ -> pure 0
{-# NOINLINE countWhileHoldingResults #-}

--------------------------------------------------------------------------------
-- Memory

{- | Leak checks. Each runs an operation many times and compares the
engine's own count of allocated bytes before and after, once the Haskell
garbage collector has released what the operation created.
-}
memory :: Group
memory =
  Group
    "memory"
    [
      ( "the engine reports its memory use"
      , withTests 1 . property $ do
          -- Zero would mean this Godot build does not track it, and the
          -- checks below would pass without testing anything.
          used <- evalIO engineMemory
          assert (used > 0)
      )
    ,
      ( "freed nodes are released"
      , releases 5000 $ \_ -> Node3D.construct >>= freeObject
      )
    ,
      ( "collected reference-counted objects are released"
      , -- The plain base class: resources such as meshes also hold memory
        -- in the rendering server, which is not returned until frames run.
        releases 5000 $ \_ -> void RefCounted.construct
      )
    ,
      ( "arrays and dictionaries are released"
      , releases 5000 $ \i -> do
          array <- arrayFromList [fromIntegral i, 2, 3 :: Int64]
          _ <- arrayToList @Int64 array
          dictionary <- dictionaryFromList [(fromIntegral i :: Int64, Vector3 1 2 3)]
          _ <- dictionaryToList @Int64 @Vector3 dictionary
          pure ()
      )
    ,
      ( "packed arrays are released"
      , releases 5000 $ \i -> do
          floats <- packedFromList @PackedFloat32Array (replicate 64 (fromIntegral i))
          _ <- packedToList floats
          strings <- packedStringsFromList ["one", show i]
          _ <- packedStringsToList strings
          pure ()
      )
    ,
      ( "callables are released, connected or not"
      , releases 5000 $ \_ -> do
          unused <- newCallable_ (\_ -> pure ())
          _ <- toVariant unused
          node <- Node3D.construct
          noArguments <- newDefault
          name <- toGString "pulse"
          Object.addUserSignal node name noArguments
          signal <- toStringName "pulse"
          connected <- newCallable_ (\_ -> pure ())
          _ <- Object.connect node signal connected 0
          freeObject node
      )
    ,
      ( "strings and their variants are released"
      , releases 20000 $ \i -> do
          string <- toGString ("a string long enough to need its own allocation " <> show i)
          variant <- toVariant string
          _ <- fromVariant @GString variant
          _ <- fromGString string
          pure ()
      )
    ,
      ( "string names are released"
      , releases 20000 $ \i -> do
          -- A different name each time, so each one is a new table entry.
          _ <- toStringName ("leak_check_" <> show i) >>= fromStringName
          pure ()
      )
    ,
      ( "value variants are released"
      , releases 20000 $ \i -> do
          _ <- toVariant (Vector3 (fromIntegral i) 2 3) >>= viewVariant
          pure ()
      )
    ,
      ( "dynamic calls release their arguments and results"
      , releases 5000 $ \i -> do
          node <- Node3D.construct
          setPosition <- toStringName "set_position"
          argument <- toVariant (Vector3 (fromIntegral i) 5 6)
          _ <- Object.call node setPosition [argument]
          freeObject node
      )
    ]

{- | The operation, run that many times, leaves the engine's memory use where
it was. Even one small allocation leaked per iteration is far above the
slack allowed here.
-}
releases :: Int -> (Int -> IO ()) -> Property
releases iterations operation = withTests 1 . property $ do
  growth <- evalIO $ do
    let run = forM_ [1 .. iterations] operation
        limit = 64 * 1024
    -- Once first, so that method handles and caches are already allocated.
    run
    replicateM_ 5 settle
    before <- engineMemory
    run
    settleUntil ((< limit) . subtract before <$> engineMemory)
    after <- engineMemory
    pure (after - before)
  annotateShow growth
  assert (growth < 64 * 1024)

{- | Collect garbage and hand back to Godot what that frees.

The values Godot must destroy are released from finalizers, which the
runtime runs shortly after a collection, not during it; hence the pause.
-}
settle :: IO ()
settle = do
  performMajorGC
  threadDelay 10000
  releasePending

-- | 'settle' until the condition holds, giving up after a couple of seconds.
settleUntil :: IO Bool -> IO ()
settleUntil done = go (200 :: Int)
  where
    go 0 = pure ()
    go n = do
      settle
      finished <- done
      unless finished (go (n - 1))

-- | Bytes currently allocated by the engine.
engineMemory :: IO Int64
engineMemory = OS.singleton >>= OS.getStaticMemoryUsage

foreign export ccall
  hs_tests_init ::
    GDExtensionInterfaceGetProcAddress ->
    GDExtensionClassLibraryPtr ->
    Ptr GDExtensionInitialization ->
    IO GDExtensionBool
