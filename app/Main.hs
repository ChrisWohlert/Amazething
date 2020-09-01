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
main = getStdGen >>= animateMaze . reverseBacktracking [(x, y) | x <- [0 .. 5], y <- [0 .. 5]]


animateMaze :: LinkedNode Node -> IO ()
animateMaze node = reanimate $ docEnv $ sceneAnimation $ play $ drawNode N (0, -1) node

drawNode :: Direction -> Node -> LinkedNode Node -> Animation
drawNode dir prev n@(LinkedNode pos nbs) = (foldl andThen (pause 0) . map drawWall . getWalls (uncurry V2 prev) $ (uncurry V2 <$> n)) `andThen` (foldl andThen (pause 0) . map (drawNode N pos) $ nbs)
drawNode _ _ (Blocked _) = pause 0

drawWall (V2 x y, V2 x' y') = mkAnimation 1 $ \ t -> withStrokeWidth 0.05 . scale 0.4 $ mkLine (x, y) (x', y')
