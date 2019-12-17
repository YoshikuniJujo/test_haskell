{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BatchedQueue (
	BatchedQueue, empty, snoc, uncons, head, tail, snocAll, consAll ) where

import Prelude hiding (head, tail)

import Queue

data BatchedQueue a = BatchedQueue [a] [a] deriving Show

instance Queue BatchedQueue where
	empty = BatchedQueue [] []
	snoc (BatchedQueue f r) x = BatchedQueue f (x : r)
	uncons (BatchedQueue [] []) = Nothing
	uncons (BatchedQueue (x : f) r) = Just (x, BatchedQueue f r)
	uncons (BatchedQueue [] r) = uncons $ BatchedQueue (rev r) []

instance ConsQueue BatchedQueue where
	cons x (BatchedQueue f r) = BatchedQueue (x : f) r

rev :: [a] -> [a]
rev = rv []
	where
	rv s [] = s
	rv s (x : xs) = rv (x : s) xs
