{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Reimplementation.BankersQueue (
	BankersQueue, empty, snoc, uncons, isEmpty, head, tail, showBQ ) where

import Prelude hiding (head, tail)

import Reimplementation.Queue
import Reimplementation.NeverOccur

import Tools.ShowThunk

data BankersQueue a = BankersQueue [a] [a] [a] deriving Show

instance Queue BankersQueue where
	empty = BankersQueue [] [] []
	snoc (BankersQueue f r []) x =
		let f' = f ++ reverse (x : r) in BankersQueue f' [] f'
	snoc (BankersQueue f r (_ : s)) x = BankersQueue f (x : r) s
	uncons (BankersQueue f r []) = case f ++ reverse r of
		[] -> Nothing
		x : f' -> Just (x, BankersQueue f' [] f')
	uncons (BankersQueue (x : f) r (_ : s)) = Just (x, BankersQueue f r s)
	uncons (BankersQueue [] _ (_ : _)) = neverOccur

showBQ :: Show a => BankersQueue a -> String
showBQ (BankersQueue f r s) = "BankersQueue (" ++ showLazyList f ++ ") (" ++ showLazyList r ++ ") (" ++ showLazyList s ++ ")"
