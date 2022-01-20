module MazeGenerator where

import Control.Monad.State.Lazy
import Data.Foldable (Foldable)
import Data.List
import qualified Data.Map as M
import System.Random

type Position = (Int, Int)

data Tile = Tile Position [Tile]
  deriving (Show)

getPos :: Tile -> Position
getPos (Tile pos _) = pos

genMaze = do
  let gen = mkStdGen 137
  let maze = evalState (buildMaze gen 5 5 (Tile (0, 0) [])) []
  print $ allConnections maze
  print maze

buildMaze :: RandomGen g => g -> Int -> Int -> Tile -> State [Position] Tile
buildMaze gen width height (Tile pos@(x, y) ns) = do
  let (randomPath, g) = uniformR (1, 4) gen
  modify (pos :)
  visited <- get
  let findNext = [Tile (x', y') [] | (x', y') <- [(x -1, y), (x + 1, y), (x, y -1), (x, y + 1)], notElem (x', y') visited && x' >= 0 && x' <= width && y' >= 0 && y' <= height]
  case findNext of
    [] -> return $ Tile pos ns
    _ -> do
      next <- buildMaze g width height ((!! max 0 randomPath) . cycle $ findNext)
      return (Tile pos (next : ns))

allConnections :: Tile -> [(Position, Position)]
allConnections (Tile pos ns) = map ((pos,) . getPos) ns ++ concatMap allConnections ns