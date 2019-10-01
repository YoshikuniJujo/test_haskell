{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import PerformanceProblems

import System.Environment

main :: IO ()
main = do
	putStrLn "Tree Substitution"
	lr : n_ : _ <- getArgs
	print . treeLength $ (case lr of "l" -> foldl1; "r" -> foldr1) (<--)
		(sample3 : replicate (read n_) sample) <-- Leaf

sample = Leaf

sample2 :: Tree
sample2 = Node
	(Node
		(Node Leaf Leaf)
		(Node Leaf Leaf))
	(Node
		(Node Leaf Leaf)
		(Node Leaf Leaf))

sample3 :: Tree
sample3 = Node sample2 sample2
