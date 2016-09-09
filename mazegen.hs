{-# language FlexibleContexts #-}

module MazeGen where

import System.Random
import qualified Data.Set as S
import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans
import Data.List
import Data.Maybe

class (Applicative m, Monad m) => MonadRandom m where
  nextRandomR :: Random r => (r,r) -> m r

instance MonadRandom IO where
  nextRandomR = randomRIO

instance MonadRandom m => MonadRandom (StateT s m) where
  nextRandomR = lift . nextRandomR

data Graph = Graph Int Int
data Cell  = Cell Int Int deriving (Eq, Ord, Show)
data Maze  = Maze Cell [Maze] deriving (Show)

generateMaze :: MonadRandom r  => Graph -> r Maze
generateMaze g@(Graph w h) = do
  start <- Cell <$> nextRandomR (1,w) <*> nextRandomR (1,h)
  evalStateT (generateMaze' g start) S.empty

generateMaze' :: (MonadRandom r, MonadState (S.Set Cell) r) => Graph -> Cell -> r Maze
generateMaze' graph cell = do
  modify $ S.insert cell
  cs <- pickPerm $ neighbours graph cell
  ms <- fmap catMaybes $ 
        forM cs $ \c -> do
          isVisited <- gets (S.member c)
          if isVisited 
            then return Nothing
            else Just <$> generateMaze' graph c
  return (Maze cell ms) 

pickPerm :: MonadRandom r => [a] -> r [a]
pickPerm xs = do
  i <- nextRandomR (0, length ps - 1)
  return $ ps !! i
  where
    ps = permutations xs

neighbours :: Graph -> Cell -> [Cell]
neighbours (Graph w h) (Cell x y) = 
    [Cell x' y' | 
        (x',y') <- [(x+1,y), (x-1,y), (x,y+1), (x,y-1)],
        x' <= w, x' > 0,
        y' <= h, y' > 0
        ]
