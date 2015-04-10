import Data.List

lessThan6 :: [Integer]
lessThan6 = 1 : 2 ^ 1 : 2 ^ 2 : 2 ^ 3 : 2 ^ 4 : 2 ^ 5 :
	zipWith6 (\a b c d e f -> a + b + c + d + e + f)
		lessThan6
		(drop 1 lessThan6)
		(drop 2 lessThan6)
		(drop 3 lessThan6)
		(drop 4 lessThan6)
		(drop 5 lessThan6)

lessThan5 :: [Integer]
lessThan5 = 1 : 2 ^ 1 : 2 ^ 2 : 2 ^ 3 : 2 ^ 4 :
	zipWith5 (\a b c d e -> a + b + c + d + e)
		lessThan5
		(drop 1 lessThan5)
		(drop 2 lessThan5)
		(drop 3 lessThan5)
		(drop 4 lessThan5)

just5 :: [Integer]
just5 = zipWith (-) lessThan6 lessThan5

main :: IO ()
main = print $ just5 !! 30
