{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- https://oeis.org/A048341

import Numeric.Natural

toDigits :: Natural -> [Natural]
toDigits 0 = []; toDigits n = n `mod` 10 : toDigits (n `div` 10)

calc :: Natural -> Natural
calc n = sum $ zipWith (^) (reverse ds) ds where ds = toDigits n

search :: Natural -> [Natural]
search mx = filter (\n -> n == calc n) [0 .. mx]
