module WarshallFloyd (
	route,
	makeDists,
	show2D
) where

import TDList

next :: Int -> [[Int]] -> [[Int]]
next v d = flip map [0 .. length d - 1] $ \f ->
	flip map [0 .. length d - 1] $ \t ->
		if d !!! (f, t) > d !!! (f, v) + d !!! (v, t)
			then d !!! (f, v) + d !!! (v, t)
			else d !!! (f, t)

route :: [[Int]] -> [[Int]]
route d = foldr next d [0 .. length d - 1]

makeDists :: Int -> Int -> [[Int]] -> [[Int]]
makeDists m s = foldl (!!!=) (replicate s $ replicate s 999) .
	map (\[i, j, v] -> (i - m, j - m, v))
