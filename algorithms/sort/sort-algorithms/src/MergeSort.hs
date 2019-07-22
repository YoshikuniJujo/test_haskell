{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MergeSort where

import Data.Array

mergeSort :: (Ix i, Integral i, Ord a) => Array i a -> Array i a
mergeSort a
	| mx < mn = array (mn, mn - 1) []
	| mn == mx = array (mn, mn) [(mn, a ! mn)]
	| otherwise = merge
		(mergeSort $ ixmap (mn, md) id a)
		(mergeSort $ ixmap (md + 1, mx) id a)
	where
	(mn, mx) = bounds a
	md = (mn + mx) `div` 2

merge :: (Ix i, Num i, Enum i, Ord a) => Array i a -> Array i a -> Array i a
merge a1 a2 = listArray (mn1, mx2) $ mrg mn1 mn2
	where
	(mn1, mx1) = bounds a1
	(mn2, mx2) = bounds a2
	mrg i j	| i > mx1 = (a2 !) <$> [j .. mx2]
		| j > mx2 = (a1 !) <$> [i .. mx1]
		| x <= y = x : mrg (i + 1) j
		| otherwise = y : mrg i (j + 1)
		where x = a1 ! i; y = a2 ! j
