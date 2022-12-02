{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MergeSort.Wow where

import Data.Bool

sort :: Ord a => [a] -> [a]
sort = \case
	[] -> []
	xs -> head . head . dropWhile multi . iterate pairs $ (: []) <$> xs

multi :: [a] -> Bool
multi = \case [_] -> False; _ -> True

pairs :: Ord a => [[a]] -> [[a]]
pairs = \case [] -> []; [xs] -> [xs]; (xs : ys : xss) -> merge xs ys : pairs xss

merge :: Ord a => [a] -> [a] -> [a]
merge [] bs = bs; merge as [] = as
merge aa@(a : as) ba@(b : bs) = bool (a : merge as ba) (b : merge aa bs) (a > b)
