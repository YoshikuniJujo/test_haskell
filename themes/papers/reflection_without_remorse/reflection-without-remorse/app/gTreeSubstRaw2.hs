{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import System.Environment

import PerformanceProblems

main :: IO ()
main = do
	n_ : _ <- getArgs
	putStrLn "Generic Tree Substitution"
	let	t = foldl1 (>>) $ replicate (read n_) sample
	print . gTreeLength $! t

sample :: GTree Int
sample = GNode (GLeaf 321) (GLeaf 123)
