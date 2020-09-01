module Main where

import MazeGenerator
import Reanimate
import Reanimate.Builtin.Documentation
import System.Random
import Linear.V2

data Direction = N | E | S | W

main :: IO ()
main = getStdGen >>= animateMaze . reverseBacktracking [(x, y) | x <- [0 .. 2], y <- [0 .. 2]]


animateMaze :: LinkedNode Node -> IO ()
animateMaze node = reanimate $ docEnv $ sceneAnimation $ drawNode N node

drawNode :: Direction -> LinkedNode Node -> Scene s ()
drawNode dir n@(LinkedNode pos nbs) = do
    mapM_ (play . drawWall) $ getWalls (uncurry V2 <$> n)
    mapM_ (drawNode N) nbs
drawNode _ (Blocked _) = wait 0

drawWall ((V2 x y), (V2 x' y')) = mkAnimation 1 $ \ t -> mkLine (x, y) (x', y') # translate x y # withStrokeWidth 0.1
