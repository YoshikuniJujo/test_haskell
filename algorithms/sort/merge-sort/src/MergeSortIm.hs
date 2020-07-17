{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MergeSortIm where

import Control.Arrow
import System.Random

type LenList a = (Int, [a])

popSorted :: Ord a => [a] -> (Maybe (LenList a), [a])
popSorted = \case
	[] -> (Nothing, [])
	[x] -> (Just $ singleton x, [])
	x : y : xs
		| x <= y -> Just `first` asc y (x `cons`) xs
		| otherwise -> Just `first` desc y (singleton x) xs

singleton :: a -> LenList a
singleton x = (1, [x])

cons :: a -> LenList a -> LenList a
cons x (n, xs) = (n + 1, x : xs)

asc :: Ord a => a -> (LenList a -> LenList a) -> [a] -> (LenList a, [a])
asc x s = \case
	[] -> (s $ singleton x, [])
	xa@(y : xs)
		| x <= y -> asc y (s . (x `cons`)) xs
		| otherwise -> (s $ singleton x, xa)

desc :: Ord a => a -> LenList a -> [a] -> (LenList a, [a])
desc x s = \case
	[] -> (x `cons` s, [])
	xa@(y : xs)
		| x <= y -> (x `cons` s, xa)
		| otherwise -> desc y (x `cons` s) xs

sample1 :: [Int]
sample1 = take 100 . randomRs (1, 100) $ mkStdGen 8

len :: LenList a -> Int
len (n, _) = n

list :: LenList a -> [a]
list (_, xs) = xs

merge :: Ord a => LenList a -> LenList a -> LenList a
merge (nl, xs0) (nr, ys0) = (nl + nr, xs0 `mrg` ys0)
	where
	xs `mrg` [] = xs
	[] `mrg` ys = ys
	xa@(x : xs) `mrg` ya@(y : ys)
		| x <= y = x : xs `mrg` ya
		| otherwise = y : xa `mrg` ys

sortWithLeft :: Ord a => Maybe Int -> LenList a -> [a] -> (LenList a, [a])
sortWithLeft (Just ms) l xs | len l >= ms = (l, xs)
sortWithLeft ms l xs = let (mlf, xs') = popSorted xs in case mlf of
	Nothing -> (l, xs')
	Just lf -> let (r, xs'') = sortWithLeft (Just $ len l) lf xs' in
		sortWithLeft ms (l `merge` r) xs''

mergesortim :: Ord a => [a] -> [a]
mergesortim xs = let (mlf, xs') = popSorted xs in case mlf of
	Nothing -> []
	Just lf -> list . fst $ sortWithLeft Nothing lf xs'
