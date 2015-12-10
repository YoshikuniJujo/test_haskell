triangle :: [[Integer]]
triangle = iterate next [1]

next :: [Integer] -> [Integer]
next ns@(_ : tns) = 1 : zipWith (+) ns tns ++ [1]

triangle' :: [[Integer]]
triangle' = iterate next' [1]

next' :: [Integer] -> [Integer]
next' ns@(_ : tns) = 1 : zipWith (+) (half ns) tns ++ [1]

half :: [Integer] -> [Integer]
half ns	| odd l = ns
	| otherwise = take (l `div` 2 - 1) ns ++ [0] ++ drop (l `div` 2) ns
	where
	l = length ns

main :: IO ()
main = do
	print $ 2 * triangle' !! 8 !! 4
	print $ 2 * triangle' !! 20 !! 10
	print $ 2 * triangle' !! 40 !! 20
