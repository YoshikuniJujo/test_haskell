
binTrees :: Integer -> Integer
binTrees 0 = 1
binTrees n = sum [ binTrees l * binTrees r | l <- [0 .. n - 1], let r = n - l - 1 ]

bt :: [Integer] -> Int -> Integer
bt xs n = sum [ xs !! l * xs !! r | l <- [0 .. n - 1], let r = n - l - 1 ]

memoization :: [a] -> ([a] -> Int -> a) -> [a]
memoization is f = xs where xs = is ++ map (f xs) [length is ..]
