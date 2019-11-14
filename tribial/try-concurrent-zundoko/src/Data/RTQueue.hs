{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.RTQueue (
	RTQueue, empty, snoc, uncons, cons, isEmpty, head, tail ) where

import Prelude hiding (head, tail)

import GHC.ShowLazyList (showLazyList)
import Control.Exception.NeverOccur (neverOccur)
import Data.Bool (bool)
import Data.List (intercalate)
import Data.Queue (Queue(..), ConsQueue(..), isEmpty, head, tail)

data RTQueue a = RTQueue [a] ![a] [a]

instance Show a => Show (RTQueue a) where
	show (RTQueue f r _) = "[" ++
		comma ss ++ bool "..." mcm cmpl ++ comma (show <$> reverse r) ++
		"]"
		where
		(cmpl, ss) = showLazyList f
		comma = intercalate ","
		mcm = case r of [] -> ""; _ : _ -> ","

instance Queue RTQueue where
	empty = RTQueue [] [] []
	snoc (RTQueue f r []) x = RTQueue f' [] f'
		where f' = rotate f (x : r) []
	snoc (RTQueue f r (_ : s)) x = RTQueue f (x : r) s
	uncons (RTQueue [] [] []) = Nothing
	uncons (RTQueue (x : f) r []) = Just (x, RTQueue f' [] f')
		where f' = rotate f r []
	uncons (RTQueue (x : f) r (_ : s)) = Just (x, RTQueue f r s)
	uncons (RTQueue [] [] (_ : _)) = neverOccur
	uncons (RTQueue [] (_ : _) _) = neverOccur

rotate :: [a] -> [a] -> [a] -> [a]
rotate [] ys a = reverse ys ++ a
rotate xs [] a = xs ++ a
rotate (x : xs) (y : ys) a = x : rotate xs ys (y : a)

instance ConsQueue RTQueue where
	cons x (RTQueue f r s) = RTQueue (x : f) r s
