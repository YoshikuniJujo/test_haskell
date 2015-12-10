triangle :: [[Integer]]
triangle = iterate next [1]

next :: [Integer] -> [Integer]
next ns@(_ : tns) = 1 : zipWith (+) (half0 ns) tns ++ [1]

half0 :: [Integer] -> [Integer]
half0 ns
	| even l = take (l `div` 2 - 1) ns ++ [0] ++ drop (l `div` 2) ns
	| otherwise = ns
	where
	l = length ns

main :: IO ()
main = do
	print $ 2 * triangle !! 20 !! 10
	print $ 2 * triangle !! 40 !! 20
