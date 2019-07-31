{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

stSortList :: Ord a => [a] -> [a]
stSortList [] = []
stSortList [x] = [x]
stSortList (x : xs@(y : ys))
	| x > y = stSortList $ x : ys
	| otherwise = x : stSortList xs

bwsort :: Ord a => [a] -> [a]
bwsort [] = []
bwsort [x] = [x]
bwsort (x : xs@(y : ys))
	| x > y = x : bwsort (x : ys)
	| otherwise = x : bwsort xs
