{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module RealtimeQueue (RTQueue, empty, snoc, uncons, cons, head, tail) where

import Prelude hiding (head, tail)

import Control.Monad (when)
import Data.Bool (bool)
import Data.List (intercalate)

import Queue (Queue(..), ConsQueue(..), head, tail)
import ShowLazyList (showLazyList)

import Printable (Printable(..))

rotate :: [a] -> [a] -> [a] -> [a]
rotate [] ys a = reverse ys ++ a
rotate xs [] a = xs ++ a
rotate (x : xs) (y : ys) a = x : rotate xs ys (y : a)

data RTQueue a = RTQueue [a] [a] [a]

showRTQueue :: Show a => RTQueue a -> IO String
showRTQueue (RTQueue f r _) = do
	(ef, sf) <- showLazyList f
	(er, sr) <- showLazyList r
	when (not er) $ error "rear list should not have thunk"
	pure $ "RTQueue [" ++ intercalate "," sf ++
		bool ".." "|" ef ++ intercalate "," (reverse sr) ++ "]"

instance Show a => Printable (RTQueue a) where show' = showRTQueue

instance Queue RTQueue where
	empty = RTQueue [] [] []
	snoc (RTQueue f r []) x = RTQueue f' [] f'
		where f' = rotate f (x : r) []
	snoc (RTQueue f r (_ : s)) x = RTQueue f (x : r) s
	uncons (RTQueue [] [] []) = Nothing
	uncons (RTQueue (x : f) r []) = Just (x, RTQueue f' [] f')
		where f' = rotate f r []
	uncons (RTQueue (x : f) r (_ : s)) = Just (x, RTQueue f r s)
	uncons (RTQueue [] [] (_ : _)) = error "never occur"
	uncons (RTQueue [] (_ : _) _) = error "never occur"

instance ConsQueue RTQueue where
	cons x (RTQueue f r s) = RTQueue (x : f) r (x : s)
