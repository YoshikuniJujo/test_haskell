parens :: [Integer] -> Int -> Integer
parens _ n | n < 1 = 1
parens l n = sum [ l !! h * l !! t | h <- [0 .. n - 1], let t = n - h - 1 ]

parensL :: [Integer]
parensL = memoization parens

memoization :: ([a] -> Int -> a) -> [a]
memoization f = xs
	where xs = map (f xs) [0 ..]

fib :: [Integer] -> Int -> Integer
fib _ n | n < 2 = fromIntegral n
fib l n = l !! (n - 1) + l !! (n - 2)
