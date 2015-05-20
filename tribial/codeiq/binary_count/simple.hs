import Data.List
import Data.Bits

toBinary :: Integer -> [Bool]
toBinary = unfoldr $ \n -> case n of
	0 -> Nothing
	_ -> Just (odd n, shiftR n 1)

countBits :: Integer -> Int
countBits = length . filter id . toBinary

f :: Integer -> Int
f = sum . map countBits . enumFromTo 1

good :: Integer -> Int
good 0 = 0
good n	| odd n = fromIntegral (n + 1) `div` 2 + 2 * good (n `div` 2)
	| otherwise = countBits n + good (n - 1)
