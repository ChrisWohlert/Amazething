module Main where

import MazeGenerator
import Reanimate
import Reanimate.Builtin.Documentation

main :: IO ()
main = reverseBacktracking [(x, y) | x <- [0 .. 2], y <- [0 .. 2]] >>= animateMaze


animateMaze :: LinkedNode Node -> IO ()
animateMaze node = reanimate $ docEnv $ mkAnimation 5 (\ t -> foldMap (mkCircle 1 # translate) node)