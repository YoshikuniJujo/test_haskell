{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BankersQueue (BankersQueue, empty, snoc, uncons, head, tail) where

import Prelude hiding (head, tail)

import Data.Bool
import Data.List (intercalate)
import System.IO.Unsafe

import GHC.Exts.Heap

import Queue
import ShowLazyList

data BankersQueue a = BankersQueue [a] ![a] [a]

instance Queue BankersQueue where
	empty = BankersQueue [] [] []
	snoc (BankersQueue f r []) x =
		let f' = f ++ reverse (x : r) in BankersQueue f' [] f'
	snoc (BankersQueue f r (_ : s)) x = BankersQueue f (x : r) s
	uncons (BankersQueue f r []) = case f ++ reverse r of
		[] -> Nothing
		x : f' -> Just (x, BankersQueue f' [] f')
	uncons (BankersQueue (x : f) r (_ : s)) = Just (x, BankersQueue f r s)
	uncons (BankersQueue [] _ (_ : _)) = error "never occur"

showBankersQueue :: Show a => BankersQueue a -> IO String
showBankersQueue (BankersQueue f r _) = do
	(b1, sf) <- showLazyList f
	(b2, sr) <- showLazyList r
--	print =<< getClosureData r
	print b1
	print sf
	print b2
	print sr
	pure $ "BankersQueue [" ++ intercalate "," sf ++ bool "," " .. " (not (b1 && b2)) ++ intercalate "," (reverse sr) ++ "]"

instance Show a => Show (BankersQueue a) where
	show = unsafePerformIO . showBankersQueue
