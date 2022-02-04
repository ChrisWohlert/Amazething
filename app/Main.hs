{-# LANGUAGE NegativeLiterals #-}

module Main where

import Control.Monad.State.Lazy
import Data.Foldable (Foldable)
import Data.Function
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Debug.Trace as Debug
import Reanimate
import System.Random

type Position = (Double, Double)

data Tile = Tile Position [Tile]
  deriving (Show)

data Wall = Wall (Position, Position) [Wall]

main :: IO ()
main = genMaze

getPos :: Tile -> Position
getPos (Tile pos _) = pos

genMaze = do
  let gen = mkStdGen 137
  let Just maze = evalState (buildMaze gen 10 10 (Tile (0, 0) [])) []
  reanimate . animateMaze $ maze

animateMaze :: Tile -> Animation
animateMaze tile = do
  let allCorridors = extendDeadEnds $ execState (corridors tile) []
  let anims =
        map (\a -> animCorridor a `parA` animWalkway a) (Debug.trace (show allCorridors) allCorridors)
  addStatic (mkBackground "gray") (foldl andThen (pause 0) anims)
  where
    corridors :: Tile -> State [[Position]] ()
    corridors (Tile pos []) = modify ([pos] :)
    corridors c@(Tile pos (n:ns)) = do
      corridors n
      forM_ ns (\ next -> modify ([pos]:) >> corridors next)
      cs <- get
      case cs of
        [] -> put [[pos]]
        (current : rest) -> put ((pos : current) : rest)
    drawCorridor color width t a =
      mkLinePath a
        & withStrokeColor color
          . partialSvg t
          . withFillOpacity 0
          . withStrokeWidth width
          . scale 0.3
          . translate -5 -5
    animWalkway a = mkAnimation (genericLength a / 15) $ \t -> drawCorridor "white" 0.6 t a
    animCorridor a = mkAnimation (genericLength a / 15) $ \t -> drawCorridor "black" 0.8 t a

extendDeadEnds :: [[Position]] -> [[Position]]
extendDeadEnds cors = map (reverse . extend . reverse . extend) cors
  where
    extend ((x, y):(x', y'):rest ) = ((0.5 * (x - x')) + x, (0.5 * (y - y')) + y):(x', y'):rest
    extend cors = cors

buildMaze :: RandomGen g => g -> Double -> Double -> Tile -> State [Position] (Maybe Tile)
buildMaze gen width height (Tile pos@(x, y) ns) = do
  let (randomPath, g) = uniformR (1, 4) gen
  visited <- get
  if notElem (x, y) visited && x >= 0 && x <= width && y >= 0 && y <= height
    then do
      modify (pos :)
      let next = take 4 . drop randomPath $ cycle [Tile (x', y') [] | (x', y') <- [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]]
      nbs <- catMaybes <$> traverse (buildMaze g width height) next
      return $ Just (Tile pos nbs)
    else return Nothing