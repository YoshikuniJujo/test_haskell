{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.PackageMerge (run) where

import Control.Arrow
import Data.List qualified as L
import Data.Map qualified as Map

run :: (Ord a, Ord frq, Num frq, Num ln) => Int -> [(a, frq)] -> Map.Map a ln
run n xs = fin (Map.empty) $ iterate (step xs') xs' !! n
	where
	xs' = ((: []) `first`) <$> L.sortOn snd xs

fin :: (Ord k, Num a) => Map.Map k a -> [([k], b)] -> Map.Map k a
fin m [] = m
fin m [_] = m
fin m (a : b : as) = fin (addForList (addForList m (fst a)) (fst b)) as

addForList :: (Ord k, Num a) => Map.Map k a -> [k] -> Map.Map k a
addForList m [] = m
addForList m (x : xs) = addForList (Map.alter (\case Nothing -> Just 1; Just n -> Just $ n + 1) x m) xs

step :: (Ord b, Num b) => [([a], b)] -> [([a], b)] -> [([a], b)]
step x = merge x . pairs

merge :: Ord b => [(a, b)] -> [(a, b)] -> [(a, b)]
merge [] bs = bs
merge as [] = as
merge kva@(kv@(_, v) : kvs) kva'@(kv'@(_,  v') : kvs')
	| v <= v' = kv : merge kvs kva'
	| otherwise = kv' : merge kva kvs'

pairs :: Num b => [([a], b)] -> [([a], b)]
pairs [] = []
pairs [_] = []
pairs ((k, v) : (k', v') : kvs) = (k ++ k', v + v') : pairs kvs
