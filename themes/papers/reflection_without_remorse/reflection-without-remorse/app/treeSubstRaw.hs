{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import PerformanceProblems

main :: IO ()
main = do
	putStrLn "Tree Substitution"
	print . treeLength $ foldl1 (<--) (replicate 9 sample) <-- Leaf

sample :: Tree
sample = Node
	(Node
		(Node Leaf Leaf)
		(Node Leaf Leaf))
	(Node
		(Node Leaf Leaf)
		(Node Leaf Leaf))
