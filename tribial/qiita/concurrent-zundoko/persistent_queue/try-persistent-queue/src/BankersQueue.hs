{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BankersQueue (BankersQueue, empty, snoc, uncons, cons, head, tail) where

import Prelude hiding (head, tail)

import Control.Monad (when)
import Data.Bool (bool)
import Data.List (intercalate)

import Queue (Queue(..), ConsQueue(..), head, tail)
import ShowLazyList (showLazyList)

import Printable (Printable(..))

data BankersQueue a = BankersQueue Int [a] Int [a]

instance Queue BankersQueue where
	empty = BankersQueue 0 [] 0 []
	snoc (BankersQueue lf f lr r) x
		| lf <= lr =
			BankersQueue (lf + lr + 1) (f ++ reverse (x : r)) 0 []
		| otherwise = BankersQueue lf f (lr + 1) (x : r)
	uncons (BankersQueue 0 _ 0 _) = Nothing
	uncons (BankersQueue lf (x : f) lr r)
		| lf <= lr = Just
			(x, BankersQueue (lf + lr - 1) (f ++ reverse r) 0 [])
		| otherwise = Just (x, BankersQueue (lf - 1) f lr r)
	uncons (BankersQueue _ [] _ _) = error "never occur"

instance ConsQueue BankersQueue where
	cons x (BankersQueue lf f lr r) = BankersQueue (lf + 1) (x : f) lr r

showBankersQueue :: Show a => BankersQueue a -> IO String
showBankersQueue (BankersQueue _ f _ r) = do
	(ef, sf) <- showLazyList f
	(er, sr) <- showLazyList r
	when (not er) $ error "rear list should not be thunk"
	pure $ "BankersQueue [" ++ intercalate "," sf ++
		bool ".." "|" ef ++ intercalate "," (reverse sr) ++ "]"

instance Show a => Printable (BankersQueue a) where show' = showBankersQueue
