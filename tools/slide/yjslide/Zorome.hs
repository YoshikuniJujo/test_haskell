module Zorome where

import Control.Applicative

isZorome ds = and $ zipWith (==) ds (tail ds)
-- isZorome :: String -> Bool
-- isZorome = and . (zipWith (==) <$> id <*> tail)
zoromes = filter (isZorome . show) [1 ..]

ones = 1 : map ((+ 1) . (* 10)) ones
zoromes2 = concatMap (\o -> map (o *) [1 .. 9]) ones

ones2 = iterate ((+ 1) . (* 10)) 1
zoromes3 = [o * n | o <- ones2, n <- [1 .. 9]]

sumZoromes n = sum $ takeWhile (<= n) zoromes3
