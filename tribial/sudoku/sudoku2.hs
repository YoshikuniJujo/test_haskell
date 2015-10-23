import Data.List

together :: Int -> [Int]
together i0 = [ i | i <- [0 .. 80],
	i `div` 9 == i0 `div` 9 ||
	i `mod` 9 == i0 `mod` 9 ||
	i `div` 27 == i0 `div` 27 && i `mod` 9 `div` 3 == i0 `mod` 9 `div` 3 ]
