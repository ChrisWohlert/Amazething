#!/usr/bin/env stack

{-# LANGUAGE NegativeLiterals #-}


module Main where

import MazeGenerator
import Reanimate
import Reanimate.Builtin.Documentation
import System.Random
import Linear.V2

data Direction = N | E | S | W

main :: IO ()
main = getStdGen >>= animateMaze . reverseBacktracking [(x, y) | x <- [0 .. 15], y <- [0 .. 15]]


animateMaze :: LinkedNode Node -> IO ()
animateMaze node = reanimate $ docEnv $ sceneAnimation $ play $ drawNode N (0, -1) node

drawNode :: Direction -> Node -> LinkedNode Node -> Animation
drawNode dir prev n@(LinkedNode pos nbs) = (foldl andThen (pause 0) . map drawWall . concatMap getWalls (uncurry V2 prev) $ (uncurry V2 <$> n))
drawNode _ _ (Blocked _) = pause 0

drawWall (V2 x y, V2 x' y') = mkAnimation 0.3 $ \ t -> withStrokeWidth 0.05 . scale 0.4 $ mkLine (x, y) (x + ((x' - x) * t), y + ((y' - y) * t))
