{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MergeSortImBy where

import Control.Arrow
import System.Random

type LenList a = (Int, [a])

lessEq :: (a -> a -> Ordering) -> a -> a -> Bool
lessEq cmp x y | GT <- x `cmp` y = False | otherwise = True

popSortedBy :: (a -> a -> Ordering) -> [a] -> (Maybe (LenList a), [a])
popSortedBy cmp = \case
	[] -> (Nothing, [])
	[x] -> (Just $ singleton x, [])
	x : y : xs
		| lessEq cmp x y -> Just `first` ascBy cmp y (x `cons`) xs
		| otherwise -> Just `first` descBy cmp y (singleton x) xs

singleton :: a -> LenList a
singleton x = (1, [x])

cons :: a -> LenList a -> LenList a
cons x (n, xs) = (n + 1, x : xs)

ascBy :: (a -> a -> Ordering) -> a -> (LenList a -> LenList a) -> [a] -> (LenList a, [a])
ascBy cmp x s = \case
	[] -> (s $ singleton x, [])
	xa@(y : xs)
		| lessEq cmp x y -> ascBy cmp y (s . (x `cons`)) xs
		| otherwise -> (s $ singleton x, xa)

descBy :: (a -> a -> Ordering) -> a -> LenList a -> [a] -> (LenList a, [a])
descBy cmp x s = \case
	[] -> (x `cons` s, [])
	xa@(y : xs)
		| lessEq cmp x y -> (x `cons` s, xa)
		| otherwise -> descBy cmp y (x `cons` s) xs

sample1 :: [Int]
sample1 = take 100 . randomRs (1, 100) $ mkStdGen 8

len :: LenList a -> Int
len (n, _) = n

list :: LenList a -> [a]
list (_, xs) = xs

mergeBy :: (a -> a -> Ordering) -> LenList a -> LenList a -> LenList a
mergeBy cmp (nl, xs0) (nr, ys0) = (nl + nr, xs0 `mrg` ys0)
	where
	xs `mrg` [] = xs
	[] `mrg` ys = ys
	xa@(x : xs) `mrg` ya@(y : ys)
		| lessEq cmp x y = x : xs `mrg` ya
		| otherwise = y : xa `mrg` ys

sortWithLeft :: (a -> a -> Ordering) -> Maybe Int -> LenList a -> [a] -> (LenList a, [a])
sortWithLeft _ (Just ms) l xs | len l >= ms = (l, xs)
sortWithLeft cmp ms l xs = let (mlf, xs') = popSortedBy cmp xs in case mlf of
	Nothing -> (l, xs')
	Just lf -> let (r, xs'') = sortWithLeft cmp (Just $ len l) lf xs' in
		sortWithLeft cmp ms (mergeBy cmp l r) xs''

mergesortimBy :: (a -> a -> Ordering) -> [a] -> [a]
mergesortimBy cmp xs = let (mlf, xs') = popSortedBy cmp xs in case mlf of
	Nothing -> []
	Just lf -> list . fst $ sortWithLeft cmp Nothing lf xs'
