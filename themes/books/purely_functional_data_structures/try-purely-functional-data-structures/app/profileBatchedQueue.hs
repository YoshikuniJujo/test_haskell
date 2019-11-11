{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (head, tail)

import System.Environment

import Reimplementation.BatchedQueue

main :: IO ()
main = do
	n_ : _ <- getArgs
	let	n = read n_
		q = foldl snoc empty [1 .. n] :: BatchedQueue Int
	print . sum $ nThunks n head q

nThunks :: Int -> (a -> b) -> a -> [b]
nThunks n _ _ | n < 1 = []
nThunks n f x = f x : nThunks (n - 1) f x
