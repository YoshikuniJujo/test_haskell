{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Ch15AllTheCommonPrefixesV2 where

import Data.Bool
import Data.Maybe
import Data.Array

import qualified RealtimeQueue as RTQ

allcp, allcp' :: Eq a => [a] -> [Int]
-- allcp xs = llcp xs <$> tails xs
allcp xs = fst4 $ until (done n) (step xs) ([n], 0, 0, 1)
	where n = length xs

allcp' xs = extract $ until (done' n) (step'' n xa) (RTQ.snoc RTQ.empty n, RTQ.empty, 0, 1)
	where
	xa = listArray (0, n - 1) xs
	extract(as, _qs, _h, _k) = RTQ.elems as
	n = length xs

type State = ([Int], Int, Int, Int)

done :: Int -> State -> Bool
done n (_as, _i, _p, k) = k == n

done' :: Int -> State' -> Bool
done' n (_as, _qs, _h, k) = k == n

step :: Eq a => [a] -> State -> State
step xs (as, i, p, k)
	| k >= i + p = (snoc as a, k, a, k + 1)
	| q /= r = (snoc as (min q r), i, p, k + 1)
	| q == r = (snoc as b, k, b, k + 1)
	| otherwise = error "never occur"
	where
	q = as !! (k - i)
	r = p - (k - i)
	a = llcp xs $ drop k xs
	b = q + llcp (drop q xs) (drop (q + k) xs)

step' :: Eq a => Int -> Array Int a -> State -> State
step' n xa (as, i, p, k)
	| k >= i + p = (snoc as a, k, a, k + 1)
	| q /= r = (snoc as (min q r), i, p, k + 1)
	| q == r = (snoc as b, k, b, k + 1)
	| otherwise = error "never occur"
	where
	q = as !! (k - i)
	r = p - (k - i)
	a = llcp' n xa 0 k
	b = q + llcp' n xa q (q + k)

type State' = (RTQ.RTQueue Int, RTQ.RTQueue Int, Int, Int)

step'' :: Eq a => Int -> Array Int a -> State' -> State'
step'' n xa (as, qs, h, k)
	| k >= h = (RTQ.snoc as a, RTQ.snoc as' a, k + a, k + 1)
	| q /= r = (RTQ.snoc as m, RTQ.snoc qs' m, h, k + 1)
	| q == r = (RTQ.snoc as b, RTQ.snoc as' b, k + b, k + 1)
	| otherwise = error "never occur"
	where
	as' = fromJust $ RTQ.tail as
	(q, qs') = fromJust $ RTQ.uncons qs
	m = min q r
	r = h - k
	a = llcp' n xa 0 k
	b = q + llcp' n xa q (q + k)

snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]

llcp :: Eq a => [a] -> [a] -> Int
llcp [] _ = 0
llcp _ [] = 0
llcp (x : xs) (y : ys) = bool 0 (1 + llcp xs ys) (x == y)

llcp' :: Eq a => Int -> Array Int a -> Int -> Int -> Int
llcp' n xa j k
	| j == n || k == n = 0
	| xa ! j == xa ! k = 1 + llcp' n xa (j + 1) (k + 1)
	| otherwise = 0

fst4 :: (a, b, c, d) -> a
fst4 (x, _, _, _) = x
