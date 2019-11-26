{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BankersQueue (BankersQueue, empty, snoc, uncons, head, tail) where

import Prelude hiding (head, tail)

import Data.Bool
import Data.List (intercalate)
import System.IO.Unsafe

import Queue
import ShowLazyList

data BankersQueue a = BankersQueue Int [a] Int ![a]

instance Queue BankersQueue where
	empty = BankersQueue 0 [] 0 []
	snoc (BankersQueue lf f lr r) x
		| lf <= lr = BankersQueue (lf + lr + 1) (f ++ reverse (x : r)) 0 []
		| otherwise = BankersQueue lf f (lr + 1) (x : r)
	uncons (BankersQueue 0 _ 0 _) = Nothing
	uncons (BankersQueue lf (x : f) lr r)
		| lf <= lr = Just (x, BankersQueue (lf + lr - 1) (f ++ reverse r) 0 [])
		| otherwise = Just (x, BankersQueue (lf - 1) f lr r)
	uncons (BankersQueue _ [] _ _) = error "never occur"

showBankersQueue :: Show a => BankersQueue a -> IO String
showBankersQueue (BankersQueue _ f _ r) = do
	(b1, sf) <- showLazyList f
	(b2, sr) <- showLazyList r
	pure $ "BankersQueue [" ++ intercalate "," sf ++ bool mc ".." (not (b1 && b2)) ++ intercalate "," (reverse sr) ++ "]"
	where
	mc = case (f, r) of
		([], _) -> "|"
		(_, []) -> "|"
		_ -> "|"

instance Show a => Show (BankersQueue a) where
	show = unsafePerformIO . showBankersQueue
