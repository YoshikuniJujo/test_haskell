{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Ch2ASurpassingProblem where

-- import Data.List

msc :: Ord a => [a] -> Int
-- msc xs = maximum [scount z zs | z : zs <- tails xs]
msc = maximum . (snd <$>) . table

scount :: Ord a => a -> [a] -> Int
scount x xs = length (filter (x <) xs)

table :: Ord a => [a] -> [(a, Int)]
-- table xs = [(z, scount z zs) | z : zs <- tails xs]
table [x] = [(x, 0)]
table xs = join (m - n) (table ys) (table zs)
	where
	m = length xs
	n = m `div` 2
	(ys, zs) = splitAt n xs

join :: (Num a1, Ord a2, Eq a1) => a1 -> [(a2, a1)] -> [(a2, a1)] -> [(a2, a1)]
join 0 txs [] = txs
join _ [] tys = tys
join n txs@((x, c) : txs') tys@((y, d) : tys')
	| x < y = (x, c + n) : join n txs' tys
	| x >= y = (y, d) : join (n - 1) txs tys'
join _ _ _ = error "Oops"
