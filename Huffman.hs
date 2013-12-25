{-# language TupleSections #-}

module Huffman where

import qualified Data.Map as M
import Data.List hiding (insert)
import Data.Ord

data Tree = Leaf Char Int | Node Int Tree Tree deriving (Show)

type BitString = [Bool]
type Table = M.Map Char BitString

frequencies :: String -> M.Map Char Int
frequencies = M.fromListWith (+) . map (,1)

mkTree :: M.Map Char Int -> Tree
mkTree = build . map mkLeaf . sortBy (comparing snd) . M.toList

mkLeaf :: (Char, Int) -> Tree
mkLeaf = uncurry Leaf

mkNode :: Tree -> Tree -> Tree
mkNode t1 t2 = Node (size t1 + size t2) t1 t2

size :: Tree -> Int
size (Leaf _ i)   = i
size (Node i _ _) = i

insert :: Tree -> [Tree] -> [Tree]
insert = insertBy (comparing size)

build :: [Tree] -> Tree
build [t] = t
build (t1 : t2 : ts) = build $ insert (mkNode t1 t2) ts
build [] = error "No text"

encode :: Table -> String -> BitString
encode tbl = concatMap (tbl M.!)

decode :: Tree -> BitString -> String
decode tree = decode' tree
  where
    decode' (Leaf c _)   bs           = c : decode' tree bs
    decode' _            []           = []
    decode' (Node _ l _) (True  : bs) = decode' l bs
    decode' (Node _ _ r) (False : bs) = decode' r bs

mkBitStrings :: Tree -> Table
mkBitStrings = mkBitStrings' []
  where
    mkBitStrings' p (Leaf c _)   = M.singleton c (reverse p)
    mkBitStrings' p (Node _ l r) = mkBitStrings' (True : p) l `M.union` mkBitStrings' (False : p) r

mkTable :: String -> Table
mkTable = mkBitStrings . mkTree . frequencies

test :: String -> String
test s = decode tree . encode tbl $ s
  where
    fs   = frequencies s
    tree = mkTree fs
    tbl  = mkBitStrings tree

main :: IO ()
main = do
  source <- readFile "sample.txt"
  let
    table = mkTable source
  print table
