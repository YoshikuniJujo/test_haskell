{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Sequence

main :: IO ()
main = do
	putStrLn "Tree Substitution"
	print . treeLength $ foldl (<--.) sample3 (replicate 1000000 sample) <--. Leaf

sample :: Tree
sample = Leaf -- Node (expr Leaf) (expr Leaf)

sample2 :: Tree
sample2 = Node
	(expr (Node
		(expr (Node (expr Leaf) (expr Leaf)))
		(expr (Node (expr Leaf) (expr Leaf)))))
	(expr (Node
		(expr (Node (expr Leaf) (expr Leaf)))
		(expr (Node (expr Leaf) (expr Leaf)))))

sample3 :: Tree
sample3 = Node (expr sample2) (expr sample2)
