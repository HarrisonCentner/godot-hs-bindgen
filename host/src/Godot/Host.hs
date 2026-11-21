{-# LANGUAGE ForeignFunctionInterface #-}

{- | Run Godot inside a Haskell program.

Here Haskell is the host: @main@ is Haskell, Godot is a shared library
(libgodot), and the program decides when each frame runs.

> main = withGodot ["my-game", "--path", "project"] $ \godot ->
>   let loop = do
>         -- per-frame Haskell code
>         quit <- GodotInstance.iteration godot
>         unless quit loop
>    in loop

Godot must be created, iterated and destroyed on one operating-system
thread. A compiled program's main thread qualifies; elsewhere (for example
in GHCi) wrap the whole use in 'Control.Concurrent.runInBoundThread'.
-}
module Godot.Host (
  withGodot,
  GodotAlreadyStarted (..),
) where

import Control.Exception (Exception, bracket, throwIO)
import Control.Monad (unless, when)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Foreign
import Foreign.C (withCString)
import Godot.Api (GodotInstance, initialize)
import Godot.Api.Class.GodotInstance qualified as GodotInstance
import Godot.Api.Runtime (adopt, releaseAtShutdown, withObjectPtr)
import Godot.GDExtension.Interface
import Godot.LibGodot (libgodot_create_godot_instance, libgodot_destroy_godot_instance)
import HsBindgen.Runtime.Support.FunPtr (toFunPtr)
import System.IO.Unsafe (unsafePerformIO)

{- | Start Godot with the given command line (program name first), run the
action, then shut the engine down.

Godot's arguments are the usual ones: @--path DIR@ selects the project,
@--headless@ runs without a window. The library is an export-template
build, so it has no editor and needs a project with a main scene.

Engine classes are usable inside the action. Nothing from "Godot.Api" may
be used after it returns.

A process can do this once. Godot does not survive being created a second
time, so a second call throws 'GodotAlreadyStarted' before reaching it.
-}
withGodot :: [String] -> (GodotInstance -> IO a) -> IO a
withGodot args action = do
  alreadyStarted <- atomicModifyIORef' started (\was -> (True, was))
  when alreadyStarted (throwIO GodotAlreadyStarted)
  bracket (toFunPtr (GDExtensionInitializationFunction_Aux onLoad)) freeHaskellFunPtr $ \onLoadPtr ->
    withMany withCString args $ \cArgs ->
      withArrayLen cArgs $ \argc argv ->
        bracket (create argc argv onLoadPtr) destroy $ \godot -> do
          started <- GodotInstance.start godot
          unless started (fail "Godot.Host: the engine failed to start")
          action godot
  where
    create argc argv onLoadPtr = do
      GDExtensionObjectPtr p <-
        libgodot_create_godot_instance
          (fromIntegral argc)
          (castPtr argv)
          (GDExtensionInitializationFunction onLoadPtr)
      when (p == nullPtr) (fail "Godot.Host: could not create the Godot instance")
      adopt (castPtr p)
    destroy godot = do
      -- Let values Godot must destroy go while the engine still exists.
      releaseAtShutdown
      withObjectPtr godot (libgodot_destroy_godot_instance . GDExtensionObjectPtr . castPtr)

{- | 'withGodot' was called in a process that has already run Godot. The
engine cannot be created twice; start a new process.
-}
data GodotAlreadyStarted = GodotAlreadyStarted
  deriving stock (Show)

instance Exception GodotAlreadyStarted

-- Whether this process has started Godot, successfully or not.
started :: IORef Bool
started = unsafePerformIO (newIORef False)
{-# NOINLINE started #-}

{- | The function Godot calls while it is being created; the same role as an
extension library's entry point.
-}
onLoad ::
  GDExtensionInterfaceGetProcAddress ->
  GDExtensionClassLibraryPtr ->
  Ptr GDExtensionInitialization ->
  IO GDExtensionBool
onLoad getProcAddress library rInit = do
  initialize getProcAddress library
  -- These two callbacks live for the rest of the process.
  ignoreLevel <- toFunPtr (GDExtensionInitializeCallback_Aux (\_ _ -> pure ()))
  ignoreLevel' <- toFunPtr (GDExtensionDeinitializeCallback_Aux (\_ _ -> pure ()))
  poke
    rInit
    GDExtensionInitialization
      { gDExtensionInitialization_minimum_initialization_level = GDEXTENSION_INITIALIZATION_CORE
      , gDExtensionInitialization_userdata = nullPtr
      , gDExtensionInitialization_initialize = GDExtensionInitializeCallback ignoreLevel
      , gDExtensionInitialization_deinitialize = GDExtensionDeinitializeCallback ignoreLevel'
      }
  pure (GDExtensionBool 1)
