{- | Haskell hosting Godot: a cube, a camera and a light are created from
Haskell, and the cube is turned a little before every frame.

> hosted-example --path godot-hosted               # a window
> hosted-example --frames 120 --path godot-hosted --headless

@--frames N@ stops after N frames; everything else goes to Godot.
-}
module Main (main) where

import Control.Monad (unless)
import GHC.Clock (getMonotonicTime)
import Godot.Api
import Godot.Api.Class.BoxMesh qualified as BoxMesh
import Godot.Api.Class.Camera3D qualified as Camera3D
import Godot.Api.Class.DirectionalLight3D qualified as DirectionalLight3D
import Godot.Api.Class.Engine qualified as Engine
import Godot.Api.Class.GodotInstance qualified as GodotInstance
import Godot.Api.Class.MeshInstance3D qualified as MeshInstance3D
import Godot.Api.Class.Node qualified as Node
import Godot.Api.Class.Node3D qualified as Node3D
import Godot.Api.Class.SceneTree qualified as SceneTree
import Godot.Host (withGodot)
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  (frameLimit, godotArgs) <- splitArgs <$> getArgs
  withGodot ("hosted-example" : godotArgs) $ \godot -> do
    root <- sceneRoot

    cube <- MeshInstance3D.construct
    MeshInstance3D.setMesh cube =<< BoxMesh.construct
    camera <- Camera3D.construct
    Node3D.setPosition camera (Vector3 0 0 3)
    light <- DirectionalLight3D.construct
    Node3D.setRotation light (Vector3 (-0.8) 0.6 0)
    Node.addChild root cube False Node_INTERNAL_MODE_DISABLED
    Node.addChild root camera False Node_INTERNAL_MODE_DISABLED
    Node.addChild root light False Node_INTERNAL_MODE_DISABLED

    start <- getMonotonicTime
    let loop frame = do
          now <- getMonotonicTime
          let angle = realToFrac (now - start) :: Float
          Node3D.setRotation cube (Vector3 (angle * 0.7) angle 0)
          quit <- GodotInstance.iteration godot
          unless (quit || maybe False (frame >=) frameLimit) (loop (frame + 1))
    loop (1 :: Int)

    Vector3 {y} <- Node3D.getRotation cube
    children <- Node.getChildCount root False
    putStrLn $
      "[hosted-example] stopped; root has "
        <> show children
        <> " children, cube rotation y = "
        <> show y

-- | The root of the scene tree: the main window.
sceneRoot :: IO Window
sceneRoot = do
  engine <- Engine.singleton
  mainLoop <- maybe (fail "no main loop") pure =<< Engine.getMainLoop engine
  tree <- maybe (fail "main loop is not a SceneTree") pure =<< cast @SceneTree mainLoop
  SceneTree.getRoot tree

splitArgs :: [String] -> (Maybe Int, [String])
splitArgs = \case
  "--frames" : n : rest -> (Just (read n), rest)
  arg : rest -> (arg :) <$> splitArgs rest
  [] -> (Nothing, [])
