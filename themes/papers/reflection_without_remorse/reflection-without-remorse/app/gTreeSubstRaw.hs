{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import PerformanceProblems

main :: IO ()
main = do
	putStrLn "Generic Tree Substitution"
	print . gTreeLength $ foldl (<---) (sample 123) (replicate 8 sample) <--- GLeaf

sample :: Int -> GTree Int
sample n = GNode
	(GNode
		(GNode (GLeaf n) (GLeaf n))
		(GNode (GLeaf n) (GLeaf n)))
	(GNode	(GNode (GLeaf n) (GLeaf n))
		(GNode (GLeaf n) (GLeaf n)))
