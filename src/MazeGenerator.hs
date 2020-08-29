{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}

module MazeGenerator where

import Control.Monad.Trans.State.Lazy
import qualified Data.Set as S
import System.Random
import System.Random.Shuffle

type Node = (Double, Double)

data LinkedNode a = LinkedNode a [LinkedNode a] | Blocked deriving(Show, Functor, Foldable)

data ReverseBacktracking = ReverseBacktracking { unvisited :: S.Set Node, rGen :: StdGen }

reverseBacktracking :: [Node] -> IO (LinkedNode Node)
reverseBacktracking nodes = do
    g <- getStdGen
    return $ evalState (runReverseBacktracking $ head nodes) $ ReverseBacktracking { unvisited = S.fromList nodes, rGen = g }

runReverseBacktracking :: Node -> State ReverseBacktracking (LinkedNode Node)
runReverseBacktracking current = do
    isMember <- S.member current . unvisited <$> get
    if isMember 
        then do
            modify $ \ s -> s { unvisited = S.delete current (unvisited s) }
            nbs <- getNeighbours current
            return $ LinkedNode current nbs
        else
            return Blocked


getNeighbours (x, y) = do
    g <- rGen <$> get
    let nbs = shuffle' [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)] 4 g
    let (_, sd) = randomR (0 :: Int, 1) g
    modify $ \ s -> s { rGen = sd }
    mapM runReverseBacktracking nbs
