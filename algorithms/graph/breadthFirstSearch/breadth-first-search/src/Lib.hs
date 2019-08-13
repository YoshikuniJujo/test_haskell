{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

data BinTree a = Tip | BinTree a (BinTree a) (BinTree a) deriving Show

breadthFirstSearch :: BinTree a -> [a]
breadthFirstSearch = breadthFirstSearchGen . (: [])

breadthFirstSearchGen :: [BinTree a] -> [a]
breadthFirstSearchGen [] = []
breadthFirstSearchGen (Tip : bts) = breadthFirstSearchGen bts
breadthFirstSearchGen (BinTree x l r : bts) = x : breadthFirstSearchGen (bts ++ [l, r])

sampleTree = BinTree 123
	(BinTree 321
		(BinTree 456 Tip Tip)
		(BinTree 654 Tip Tip))
	(BinTree 333
		(BinTree 444 Tip Tip)
		(BinTree 666 Tip Tip))
