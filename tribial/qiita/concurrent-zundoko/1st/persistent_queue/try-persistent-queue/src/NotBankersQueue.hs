{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module NotBankersQueue (NotBankersQueue, empty, snoc, uncons, head, tail) where

import Prelude hiding (head, tail)

import Data.Bool
import Data.List (intercalate)
import System.IO.Unsafe

-- import GHC.Exts.Heap

import Queue
import ShowLazyList

data NotBankersQueue a = NotBankersQueue [a] ![a] [a]

instance Queue NotBankersQueue where
	empty = NotBankersQueue [] [] []
	snoc (NotBankersQueue f r []) x =
		let f' = f ++ reverse (x : r) in NotBankersQueue f' [] f'
	snoc (NotBankersQueue f r (_ : s)) x = NotBankersQueue f (x : r) s
	uncons (NotBankersQueue f r []) = case f ++ reverse r of
		[] -> Nothing
		x : f' -> Just (x, NotBankersQueue f' [] f')
	uncons (NotBankersQueue (x : f) r (_ : s)) = Just (x, NotBankersQueue f r s)
	uncons (NotBankersQueue [] _ (_ : _)) = error "never occur"

showNotBankersQueue :: Show a => NotBankersQueue a -> IO String
showNotBankersQueue (NotBankersQueue f r _) = do
	(b1, sf) <- showLazyList f
	(b2, sr) <- showLazyList r
--	print =<< getClosureData r
--	print b1
--	print sf
--	print b2
--	print sr
	pure $ "NotBankersQueue [" ++ intercalate "," sf ++ bool "," " .. " (not (b1 && b2)) ++ intercalate "," (reverse sr) ++ "]"

instance Show a => Show (NotBankersQueue a) where
	show = unsafePerformIO . showNotBankersQueue
