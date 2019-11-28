{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Ch15AllTheCommonPrefixesV2 (allcp) where

import Prelude hiding (head, tail)

import Data.Bool (bool)
import Data.Array (Array, listArray, (!))

import RealtimeQueue (RTQueue, empty, snoc, uncons, tail, elems)
import NeverOccur (neverOccur, neverNothing)

allcp :: Eq a => [a] -> [Int]
allcp xs = elems . fst $ until (done n)
	(step n $ listArray (0, n - 1) xs) (empty `snoc` n, (empty, 0, 1))
	where n = length xs

type State = (RTQueue Int, (RTQueue Int, Int, Int))

done :: Int -> State -> Bool
done n (_as, (_qs, _h, k)) = k == n

step :: Eq a => Int -> Array Int a -> State -> State
step n xa (as, (qs, h, k))
	| k >= h = (as `snoc` a, (tas `snoc` a, k + a, k + 1))
	| q /= r = (as `snoc` m, (tqs `snoc` m, h, k + 1))
	| q == r = (as `snoc` b, (tas `snoc` b, k + b, k + 1))
	| otherwise = neverOccur
	where
	tas = neverNothing $ tail as
	(q, tqs) = neverNothing $ uncons qs
	r = h - k
	m = q `min` r
	a = llcp n xa 0 k
	b = q + llcp n xa q (q + k)

llcp :: Eq a => Int -> Array Int a -> Int -> Int -> Int
llcp n xa j k
	| j == n || k == n = 0
	| otherwise = bool 0 (1 + llcp n xa (j + 1) (k + 1)) (xa ! j == xa ! k)
