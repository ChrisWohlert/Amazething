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
  let allCorridors = execState (corridors tile tile) []
  let anims =
        map (\a -> animCorridor a `parA` animWalkway a) allCorridors
  addStatic (mkBackground "white") (foldl andThen (pause 0) anims)
  where
    corridors :: Tile -> Tile -> State [[Position]] ()
    corridors (Tile pos ns) (Tile npos []) = modify ([npos] :)
    corridors c@(Tile pos ns) (Tile npos _) = do
      forM_ ns (corridors c)
      cs <- get
      case cs of
        [] -> put [[npos, pos]]
        (current : rest) -> put ((npos : pos : current) : rest)
    test (Tile pos ns) = unfoldr undefined b
    drawCorridor color width t a =
      mkLinePath a
        & withStrokeColor color
          . withFillOpacity 0
          . withStrokeWidth width
          . scale 0.3
          . translate -5 -5
          . partialSvg t
          . pathify
    animWalkway a = mkAnimation (genericLength a / 15) $ \t -> drawCorridor "white" 0.6 t a
    animCorridor a = mkAnimation (genericLength a / 15) $ \t -> drawCorridor "red" 0.8 t a

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