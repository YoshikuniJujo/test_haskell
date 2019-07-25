{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BinaryTree where

data BinTree a = Tip | BinTree a (BinTree a) (BinTree a)
	deriving Show

listToBinTree :: [a] -> BinTree a
listToBinTree = head . listToBinTrees 1

listToBinTrees :: Int -> [a] -> [BinTree a]
listToBinTrees _ [] = repeat Tip
listToBinTrees n xs = let (es, xs') = splitAt n xs in
	binTrees es $ listToBinTrees (n * 2) xs'

binTrees :: [a] -> [BinTree a] -> [BinTree a]
binTrees [] _ = []
binTrees (x : xs) ~(l : r : bts) = BinTree x l r : binTrees xs bts

takeBinTree :: Int -> BinTree a -> BinTree a
takeBinTree n _ | n < 1 = Tip
takeBinTree _ Tip = Tip
takeBinTree n (BinTree x l r) = BinTree x (takeBinTree (n - 1) l) (takeBinTree (n - 1) r)
