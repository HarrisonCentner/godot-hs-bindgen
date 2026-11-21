{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

{- | GDExtension entry point. Godot looks up 'hs_example_init' (named in
@godot/hs_example.gdextension@) when it loads the shared library.

It shows both layers: the raw C interface ("Godot.GDExtension.*") for the
entry point itself, and the engine API ("Godot.Api") for everything after.
-}
module Example.Entry (hs_example_init) where

import Control.Monad (when)
import Data.Void (Void)
import Foreign
import GHC.Clock (getMonotonicTimeNSec)
import Godot.Api
import Godot.Api.Class.Engine qualified as Engine
import Godot.Api.Class.Node qualified as Node
import Godot.Api.Class.Node3D qualified as Node3D
import Godot.GDExtension.Interface
import HsBindgen.Runtime.Support.FunPtr (toFunPtr)
import System.IO
import Text.Printf (printf)

hs_example_init ::
  GDExtensionInterfaceGetProcAddress ->
  GDExtensionClassLibraryPtr ->
  Ptr GDExtensionInitialization ->
  IO GDExtensionBool
hs_example_init getProcAddress library rInit = do
  hSetBuffering stdout LineBuffering
  initialize getProcAddress library

  -- These callbacks live as long as the library, so they are never freed.
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

{- | Called by Godot once per initialization level. Scene classes such as
@Node3D@ exist from the scene level on.
-}
onLevel :: Ptr Void -> GDExtensionInitializationLevel -> IO ()
onLevel _ level = when (level == GDEXTENSION_INITIALIZATION_SCENE) demo

demo :: IO ()
demo = do
  engine <- Engine.singleton
  architecture <- fromGString =<< Engine.getArchitectureName engine
  putStrLn ("[hs-example] running on " <> architecture)

  -- Value types are plain records.
  node <- Node3D.construct
  Node3D.setPosition node (Vector3 1 2 3)
  position@Vector3 {x} <- Node3D.getPosition node
  putStrLn ("[hs-example] position: " <> show position <> ", x = " <> show x)

  -- Node.setName is a Node method; a Node3D is accepted as it is.
  Node.setName node =<< toStringName "from_haskell"
  name <- fromStringName =<< Node.getName node
  putStrLn ("[hs-example] name: " <> name)

  -- Nullable results are Maybe.
  orphan <- Node3D.getParentNode3d node
  putStrLn ("[hs-example] parent before: " <> maybe "none" (const "some") orphan)
  child <- Node3D.construct
  Node.addChild node child False Node_INTERNAL_MODE_DISABLED
  parent <- Node3D.getParentNode3d child
  children <- Node.getChildCount node False
  putStrLn $
    "[hs-example] after addChild: "
      <> show children
      <> " child, child's parent is the node: "
      <> show (parent == Just node)

  time "Node3D.setPosition" (\i -> Node3D.setPosition node (Vector3 (fromIntegral i) 2 3))
  time "Node3D.setPosition_unsafe" (\i -> Node3D.setPosition_unsafe node (Vector3 (fromIntegral i) 2 3))

  -- Frees the child too.
  freeObject node

-- | Nanoseconds per call over a million calls.
time :: String -> (Int -> IO ()) -> IO ()
time label act = do
  let n = 1000000 :: Int
      go !i = when (i < n) (act i >> go (i + 1))
  t0 <- getMonotonicTimeNSec
  go 0
  t1 <- getMonotonicTimeNSec
  printf "[hs-example] %-26s %5.1f ns per call\n" label (fromIntegral (t1 - t0) / fromIntegral n :: Double)
{-# INLINE time #-}

foreign export ccall
  hs_example_init ::
    GDExtensionInterfaceGetProcAddress ->
    GDExtensionClassLibraryPtr ->
    Ptr GDExtensionInitialization ->
    IO GDExtensionBool
