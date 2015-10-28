import Data.List

minfree :: [Int] -> Int
minfree xs = minfrom' 0 (length xs, xs)

minfrom :: Int -> (Int, [Int]) -> Int
minfrom a (0, _) = a
minfrom a (n, xs)
	| length us == m = minfrom b (length vs, vs)
	| otherwise = minfrom a (length us, us)
	where
	m = (n + 1) `div` 2
	b = a + m
	(us, vs) = partition (< b) xs

minfrom' :: Int -> (Int, [Int]) -> Int
minfrom' a (0, _) = a
minfrom' a (n, xs)
	| m == b - a = minfrom' b (n - m, vs)
	| otherwise = minfrom' a (m, us)
	where
	b = a + 1 + n `div` 2
	(us, vs) = partition (< b) xs
	m = length us
