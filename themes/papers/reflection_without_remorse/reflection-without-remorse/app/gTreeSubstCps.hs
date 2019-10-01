{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import System.Environment

import PerformanceProblems
import ContinuationPassingStyle

main :: IO ()
main = do
	n_ : _ <- getArgs
	putStrLn "Generic Tree Substitution"
	let	t = absM
			$ foldl (>>=) (repM $ sample2 123) (replicate (read n_) $! sample1')
				>>= repM . GLeaf
	print . gTreeLength $! t
	{--
	print . gTreeLength . absM
		$ foldl (>>=) (repM $ sample 123) (replicate 8 $ repM . sample)
			>>= repM . GLeaf
			--}

sample :: Int -> GTree Int
sample = GLeaf

sample1' = repM . sample1

sample1 :: Int -> GTree Int
sample1 0 = GLeaf 0
sample1 _ = GNode
	(GNode
		(GNode (GLeaf 1) (GLeaf 0))
		(GNode (GLeaf 0) (GLeaf 0)))
	(GNode	(GNode (GLeaf 0) (GLeaf 0))
		(GNode (GLeaf 0) (GLeaf 0)))

sample2 :: Int -> GTree Int
sample2 n = GNode
	(GNode
		(GNode (GLeaf n) (GLeaf 0))
		(GNode (GLeaf 0) (GLeaf 0)))
	(GNode	(GNode (GLeaf 0) (GLeaf 0))
		(GNode (GLeaf 0) (GLeaf 0)))

sample3 :: Int -> GTree Int
sample3 n = GNode (sample2 n) (sample2 n)
