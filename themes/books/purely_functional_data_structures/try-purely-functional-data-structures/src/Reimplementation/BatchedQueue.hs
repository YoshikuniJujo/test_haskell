{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Reimplementation.BatchedQueue (
	BatchedQueue, empty, snoc, uncons, isEmpty, head, tail, showBQ ) where

import Prelude hiding (head, tail)

import Reimplementation.Queue (Queue(..), isEmpty, head, tail)

import Tools.ShowThunk

data BatchedQueue a = BatchedQueue [a] [a] deriving Show

instance Queue BatchedQueue where
	empty = BatchedQueue [] []
	snoc (BatchedQueue f r) x = BatchedQueue f (x : r)
	uncons (BatchedQueue [] r) = case reverse r of
		[] -> Nothing
		x : f' -> Just (x, BatchedQueue f' [])
	uncons (BatchedQueue (x : f) r) = Just (x, BatchedQueue f r)

showBQ :: Show a => BatchedQueue a -> String
showBQ (BatchedQueue f r) = "BatchedQueue (" ++ showLazyList f ++ ") (" ++ showLazyList r ++ ")"
