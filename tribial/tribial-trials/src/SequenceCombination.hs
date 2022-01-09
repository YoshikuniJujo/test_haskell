{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SequenceCombination where

import qualified Data.Map as M

divide :: Ord a => Int -> [a] -> [M.Map a Int]
divide 0 [] = [M.empty]
divide _ [] = []
divide 0 _ = [M.empty]
divide n xa@(x : xs)
	| n < 0 = []
	| otherwise = (plus1 x <$> divide (n - 1) xa) ++ divide n xs

plus1 :: Ord k => k -> M.Map k Int -> M.Map k Int
plus1 x = M.insertWith (+) x 1

orders :: Ord a => M.Map a Int -> [[a]]
orders src
	| M.null src = [[]]
	| otherwise = [ k : o | k <- M.keys src, o <- orders $ minus1 k src ]

minus1 :: Ord k => k -> M.Map k Int -> M.Map k Int
minus1 = M.alter $ \case
	Nothing -> Nothing
	Just n	| n > 1 -> Just $ n - 1
		| otherwise -> Nothing

pop :: Ord k => k -> M.Map k Int -> (k, M.Map k Int)
pop k m = (k, minus1 k m)

someOrder :: Ord a => Int -> [a] -> [[a]]
someOrder n xs = concatMap orders $ divide n xs

includeIt :: Ord a => a -> Int -> [a] -> [[a]]
includeIt x0 n xs = concatMap orders $ plus1 x0 <$> divide (n - 1) xs
