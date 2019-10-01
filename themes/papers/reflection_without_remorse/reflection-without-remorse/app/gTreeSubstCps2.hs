{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import System.Environment

import PerformanceProblems
import ContinuationPassingStyle

main :: IO ()
main = do
	n_ : _ <- getArgs
	putStrLn "Generic Tree Substitution"
	let	t = absM . sequence $ replicate (read n_) sample
	print . gTreeLength $! t

sample :: CodensityT GTree Int
sample = repM $ GLeaf 123
