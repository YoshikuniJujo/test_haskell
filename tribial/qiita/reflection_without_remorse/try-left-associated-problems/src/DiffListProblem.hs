{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module DiffListProblem where

import Control.Arrow

import DifferenceList

par :: [a] -> [a] -> ([(a, a)], ([a], [a]))
par xs [] = ([], (xs, []))
par [] ys = ([], ([], ys))
par (x : xs) (y : ys) = ((x, y) :) `first` par xs ys

diffPar :: DiffList a -> DiffList a -> (DiffList (a, a), (DiffList a, DiffList a))
diffPar xs ys = toDiffList *** (toDiffList *** toDiffList) $ par (fromDiffList xs) (fromDiffList ys)

addIfNotEmpty :: DiffList a -> DiffList a -> DiffList a
addIfNotEmpty xs ys
	| [] <- xs [] = toDiffList []
	| otherwise = xs . ys
