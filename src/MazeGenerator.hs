{-# LANGUAGE DeriveFunctor, DeriveFoldable, TypeSynonymInstances, FlexibleInstances #-}

module MazeGenerator where

import Control.Monad.Trans.State.Lazy
import qualified Data.Set as S
import System.Random
import System.Random.Shuffle
import Data.List
import Linear.V2

type Wall = ((Double, Double), (Double, Double))

type Node = (Double, Double)

data LinkedNode a = LinkedNode a [LinkedNode a] | Blocked a deriving(Show, Functor, Foldable)

data ReverseBacktracking a = ReverseBacktracking { unvisited :: S.Set a, rGen :: StdGen }

class Neighbour a where
    getNbs :: a -> [a]

instance Neighbour Node where
    getNbs (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

reverseBacktracking :: (Neighbour a, Ord a) => [a] -> StdGen -> LinkedNode a
reverseBacktracking nodes g = evalState (runReverseBacktracking $ head nodes) $ ReverseBacktracking { unvisited = S.fromList nodes, rGen = g }

runReverseBacktracking current = do
    isMember <- S.member current . unvisited <$> get
    if isMember 
        then do
            modify $ \ s -> s { unvisited = S.delete current (unvisited s) }
            nbs <- getNeighbours current
            return $ LinkedNode current nbs
        else
            return $ Blocked current


getNeighbours nb = do
    g <- rGen <$> get
    let nbs = shuffle' (getNbs nb) 4 g
    let (_, sd) = randomR (0 :: Int, 1) g
    modify $ \ s -> s { rGen = sd }
    mapM runReverseBacktracking nbs

getWalls (Blocked node) = []
getWalls (LinkedNode node nodes) = [findEdge node x | (Blocked x) <- nodes]

findEdge :: V2 Double -> V2 Double -> (V2  Double, V2 Double)
findEdge (V2 a b) (V2 c d) = (V2 1 1, V2 1 1)