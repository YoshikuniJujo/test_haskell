{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MergeSort where

mergeSort :: Ord a => [a] -> [a]
mergeSort = headOr [] . head
	. dropWhile multi . iterate (pairsWith merge) . map (: [])

headOr :: a -> [a] -> a
headOr d [] = d; headOr _ (x : _) = x

multi :: [a] -> Bool
multi [] = False; multi [_] = False; multi _ = True

pairsWith :: (a -> a -> a) -> [a] -> [a]
pairsWith _ [] = []; pairsWith _ [x] = [x]
pairsWith op (x : y : xs) = x `op` y : pairsWith op xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs; merge [] ys = ys
merge xa@(x : xs) ya@(y : ys)
	| x <= y = x : merge xs ya
	| otherwise = y : merge xa ys
