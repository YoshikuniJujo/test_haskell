{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.PackageMerge where

import Data.Map qualified as Map

pairs [] = []
pairs [_] = []
pairs ((k, v) : (k', v') : kvs) = (k ++ k', v + v') : pairs kvs

merge [] bs = bs
merge as [] = as
merge kva@(kv@(_, v) : kvs) kva'@(kv'@(_,  v') : kvs')
	| v <= v' = kv : merge kvs kva'
	| otherwise = kv' : merge kva kvs'

step x = merge x . pairs

fin m [] = m
fin m [_] = m
fin m (a : b : abs) = fin (addForList (addForList m (fst a)) (fst b)) abs

addForList m [] = m
addForList m (x : xs) = addForList (Map.alter (\case Nothing -> Just 1; Just n -> Just $ n + 1) x m) xs

run n xs = fin (Map.empty) $ iterate (step xs) xs !! n
