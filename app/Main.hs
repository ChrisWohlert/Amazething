module Main where

import MazeGenerator
import Reanimate
import Reanimate.Builtin.Documentation
import System.Random

data Direction = N | E | S | W

main :: IO ()
main = getStdGen >>= animateMaze . reverseBacktracking [(x, y) | x <- [0 .. 2], y <- [0 .. 2]]


animateMaze :: LinkedNode Node -> IO ()
animateMaze node = reanimate $ docEnv $ sceneAnimation $ drawNode N node

drawNode dir n@(LinkedNode pos nbs) = do
    mapM_ (play . drawWall . findEgde pos) $ getWalls n
    mapM_ (drawNode N) nbs
drawNode _ (Blocked _) = wait 0

drawWall (x, y) = mkAnimation 1 $ \ t -> mkLine (x, y) (x + 1, y + 1) # translate x y

findEgde (x, y) (x', y') = ((x' - x) / 2, (y' - y) / 2)