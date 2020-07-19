{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Original where

sort :: Ord a => [a] -> [a]
sort = sortBy compare
sortBy cmp = mergeAll . sequences
	where
	sequences (a : b : xs)
		| a `cmp` b == GT = descending b [a] xs
		| otherwise = ascending b (a :) xs
	sequences xs = [xs]

	descending a as (b : bs)
		| a `cmp` b == GT = descending b (a : as) bs
	descending a as bs = (a : as) : sequences bs

	ascending a as (b : bs)
		| a `cmp` b /= GT = ascending b (\ys -> as (a : ys)) bs
	ascending a as bs = let !x = as [a] in x : sequences bs

	mergeAll [x] = x
	mergeAll xs = mergeAll (mergePairs xs)

	mergePairs (a : b : xs) = let !x = merge a b in x : mergePairs xs
	mergePairs xs = xs

	merge as@(a : as') bs@(b : bs')
		| a `cmp` b == GT = b :merge as bs'
		| otherwise = a : merge as' bs
	merge [] bs = bs
	merge as [] = as
