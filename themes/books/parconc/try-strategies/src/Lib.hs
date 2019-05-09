{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Control.Concurrent
import Control.Parallel.Strategies
import System.IO.Unsafe

some :: Int -> Int -> (Int, Int)
some x y = runEval $ do
	a <- rpar (f x)
	b <- rpar (f y)
	return (a, b)

some' :: Int -> Int -> (Int, Int)
some' x y = runEval $ do
	a <- rpar (f x)
	b <- rseq (f y)
	return (a, b)

some'' :: Int -> Int -> (Int, Int)
some'' x y = runEval $ do
	a <- rpar (f x)
	b <- rseq (f y)
	_ <- rseq a
	return (a, b)

some''' :: Int -> Int -> (Int, Int)
some''' x y = runEval $ do
	a <- rpar (f x)
	b <- rpar (f y)
	_ <- rseq a
	_ <- rseq b
	return (a, b)

f :: Int -> Int
f x = unsafePerformIO $ do
	putStrLn $ "f " ++ show x ++ " begin"
	threadDelay 1000000
	putStrLn $ "f " ++ show x ++ " end"
	return $ 2 * x
