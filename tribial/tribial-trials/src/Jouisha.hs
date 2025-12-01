module Jouisha where

join :: (Eq a, Num a, Ord t) => a -> [(t, a)] -> [(t, a)] -> [(t, a)]
join 0 txs [] = txs
join _ [] tys = tys
join n txs@((x, c) : txs') tys@((y, d) : tys')
	| x < y = (x, c + n) : join n txs' tys
	| x >= y = (y, d) : join (n - 1) txs tys'

table :: Ord t => [t] -> [(t, Int)]
table [x] = [(x, 0)]
table xs = join (n - m) (table ys) (table zs)
	where
	n = length xs
	m = n `div` 2
	(ys, zs) = splitAt m xs
