{- | Godot running on its own thread for the life of a GHCi session, with the
scene and the per-frame code replaceable while it runs: live coding.

> ghci> import Godot.Host.Live qualified as Live
> ghci> Live.start ["live", "--path", "godot"]
> ghci> Live.setScene Scene.scene      -- and again after every :reload

The engine thread reads the frame callback from a cell on every frame. That
cell is a top-level value of this compiled package, which GHCi's @:reload@
and @:load@ do not reset; a top-level value in an interpreted module would
be.

Godot cannot be started twice in one process, so after 'stop', or after the
window is closed, GHCi has to be restarted.

Useful GHCi setup:

> :def! install \_ -> pure ":reload\nGodot.Host.Live.setScene Scene.scene"
> :def! quit \_ -> Godot.Host.Live.stop >> pure "::quit"

and start GHCi with @+RTS -N4 -RTS@, so that work at the prompt does not
stall frames.
-}
module Godot.Host.Live (
  Frame (..),
  start,
  stop,
  isRunning,
  onGodot,
  setFrame,
  setScene,
  sceneRoot,
) where

import Control.Concurrent.Async (Async, asyncBound, waitCatch, waitCatchSTM)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forM_, unless, when)
import Data.IORef
import GHC.Clock (getMonotonicTime)
import Godot.Api
import Godot.Api.Class.Engine qualified as Engine
import Godot.Api.Class.GodotInstance qualified as GodotInstance
import Godot.Api.Class.Node qualified as Node
import Godot.Api.Class.Node3D qualified as Node3D
import Godot.Api.Class.SceneTree qualified as SceneTree
import Godot.Host (GodotAlreadyStarted (..), withGodot)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

-- | What the frame callback is told about the frame about to be drawn.
data Frame = Frame
  { number :: Int
  -- ^ Frames since 'start'.
  , time :: Double
  -- ^ Seconds since 'start'.
  , delta :: Double
  -- ^ Seconds since the previous frame.
  }
  deriving stock (Show)

data Live = Live
  { frame :: IORef (Frame -> IO ())
  , jobs :: TQueue (IO ())
  -- ^ Actions to run on the engine thread before the next frame.
  , stopping :: TVar Bool
  , stage :: IORef (Maybe Node3D)
  -- ^ The node the current scene lives under.
  , reported :: IORef (Maybe String)
  -- ^ The last frame error printed, to print a repeating one only once.
  , engine :: Async ()
  }

data Phase
  = NotStarted
  | Running Live
  | Finished

-- Survives :reload because this module is compiled, not interpreted.
globalState :: MVar Phase
globalState = unsafePerformIO (newMVar NotStarted)
{-# NOINLINE globalState #-}

getGlobalState :: IO (MVar Phase)
getGlobalState = pure globalState

{- | Start Godot on a new bound thread, with the given command line (program
name first), and return once the engine is running frames.
-}
start :: [String] -> IO ()
start args = do
  state <- getGlobalState
  mask $ \restore ->
    takeMVar state >>= \case
      Running live -> do
        putMVar state (Running live)
        restore (putStrLn "Godot.Host.Live: already running")
      Finished -> do
        putMVar state Finished
        throwIO GodotAlreadyStarted
      NotStarted -> do
        -- Forked unmasked, so that the engine thread is not born masked.
        (live, up) <- restore (launch state) `onException` putMVar state NotStarted
        putMVar state (Running live)
        let Live {engine} = live
        restore $
          atomically ((Left <$> waitCatchSTM engine) `orElse` (Right <$> takeTMVar up)) >>= \case
            Right () -> pure ()
            Left (Left e) -> throwIO e
            Left (Right ()) -> throwIO (userError "Godot.Host.Live: the engine stopped while starting")
  where
    launch state = do
      frame <- newIORef (\_ -> pure ())
      jobs <- newTQueueIO
      stopping <- newTVarIO False
      stage <- newIORef Nothing
      reported <- newIORef Nothing
      up <- newEmptyTMVarIO
      engine <-
        asyncBound $
          runEngine args frame jobs stopping reported (atomically (putTMVar up ()))
            `finally` modifyMVar_ state (\_ -> pure Finished)
      pure (Live {..}, up)

runEngine ::
  [String] -> IORef (Frame -> IO ()) -> TQueue (IO ()) -> TVar Bool -> IORef (Maybe String) -> IO () -> IO ()
runEngine args frame jobs stopping reported signalUp =
  withGodot args $ \godot -> do
    began <- getMonotonicTime
    signalUp
    let loop number previous = do
          now <- getMonotonicTime
          atomically (flushTQueue jobs) >>= sequence_
          callback <- readIORef frame
          guardFrame reported (callback Frame {number, time = now - began, delta = now - previous})
          quit <- GodotInstance.iteration godot
          stop' <- readTVarIO stopping
          unless (quit || stop') (loop (number + 1) now)
    loop 0 began
    -- Drop what would keep engine objects alive past the engine.
    writeIORef frame (\_ -> pure ())

{- | Run the frame callback; an exception is printed (once while it keeps
being the same one) and the frame is drawn anyway.
-}
guardFrame :: IORef (Maybe String) -> IO () -> IO ()
guardFrame reported action =
  try action >>= \case
    Right () -> pure ()
    Left e
      | Just (SomeAsyncException _) <- fromException e -> throwIO e
      | otherwise -> do
          let message = displayException (e :: SomeException)
          previous <- readIORef reported
          when (previous /= Just message) $ do
            writeIORef reported (Just message)
            hPutStrLn stderr ("Godot.Host.Live: frame callback threw: " <> message)

running :: IO Live
running =
  getGlobalState >>= readMVar >>= \case
    Running live -> pure live
    NotStarted -> throwIO (userError "Godot.Host.Live: not started")
    Finished -> throwIO (userError "Godot.Host.Live: Godot has stopped; restart GHCi")

isRunning :: IO Bool
isRunning =
  getGlobalState >>= readMVar >>= \case
    Running _ -> pure True
    _ -> pure False

-- | Shut the engine down after the current frame and wait for it.
stop :: IO ()
stop =
  getGlobalState >>= readMVar >>= \case
    Running Live {stopping, engine} -> do
      atomically (writeTVar stopping True)
      waitCatch engine >>= either throwIO pure
    _ -> pure ()

{- | Run an action on the engine thread, between two frames, and wait for its
result. Everything that touches the scene has to go through this (or run in
the frame callback): Godot's scene tree belongs to that thread.
-}
onGodot :: IO a -> IO a
onGodot action = do
  Live {jobs, engine} <- running
  result <- newEmptyTMVarIO
  atomically (writeTQueue jobs (try action >>= atomically . putTMVar result))
  atomically ((Right <$> takeTMVar result) `orElse` (Left <$> waitCatchSTM engine)) >>= \case
    Right (Right a) -> pure a
    Right (Left e) -> throwIO (e :: SomeException)
    Left _ -> throwIO (userError "Godot.Host.Live: the engine stopped")

-- | Replace the per-frame code. Takes effect on the next frame.
setFrame :: (Frame -> IO ()) -> IO ()
setFrame callback = do
  Live {frame, reported} <- running
  writeIORef reported Nothing
  writeIORef frame callback

{- | Replace the scene: free everything the previous scene created, give the
new one an empty node to build under, and use what it returns as the
per-frame code. If building fails nothing runs per frame, and the exception
is rethrown here.
-}
setScene :: (Node3D -> IO (Frame -> IO ())) -> IO ()
setScene build = do
  Live {frame, stage, reported} <- running
  onGodot $ do
    writeIORef frame (\_ -> pure ())
    writeIORef reported Nothing
    old <- readIORef stage
    forM_ old $ \node -> do
      alive <- isAlive node
      when alive (freeObject node)
    root <- sceneRoot
    node <- Node3D.construct
    Node.setName node =<< toStringName "stage"
    Node.addChild root node False Node_INTERNAL_MODE_DISABLED
    writeIORef stage (Just node)
    writeIORef frame =<< build node

-- | The root of the scene tree: the main window.
sceneRoot :: IO Window
sceneRoot = do
  engine <- Engine.singleton
  mainLoop <- maybe (fail "no main loop") pure =<< Engine.getMainLoop engine
  tree <- maybe (fail "main loop is not a SceneTree") pure =<< cast @SceneTree mainLoop
  SceneTree.getRoot tree
