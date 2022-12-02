{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module QuickSort.Wow where

import Data.List (partition)

sort :: Ord a => [a] -> [a]
sort = \case
	[] -> []
	(x : xs) -> sort ss ++ [x] ++ sort bs
		where (ss, bs) = partition (< x) xs
