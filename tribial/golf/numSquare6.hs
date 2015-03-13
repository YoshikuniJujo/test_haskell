import Control.Arrow

main :: IO ()
main = interact $ (++ "\n") . show . checkSquares . read . head . lines

checkSquares :: Int -> Int
checkSquares n
	| any (== 0) ds = 1
	| any (`elem` ss) ds = 2
	| any (`elem` ts) ds = 3
	| otherwise = 4
	where
	ss = takeWhile (<= n) squares
	ds = map (n -) ss
	ts = takeWhile (<= n) squares2

squares, squares2 :: [Int]
squares = map (^ 2) [1 ..]
squares2 = ordered 0 [2 ..] adds

adds, adds2 :: [[Int]]
adds = map (\x -> map (\y -> x ^ 2 + y ^ 2) [x ..]) [1 ..]
adds2 = map (\x -> map (\y -> x + y) squares) squares2

takeIt :: Int -> Int -> [[Int]] -> (Bool, [[Int]])
takeIt n _ xss | n < 1 = (False, xss)
takeIt n x0 (xa@(x : xs) : xss)
	| x == x0 = (True, xs : snd (takeIt (n - 1) x0 xss))
	| otherwise = second (xa :) $ takeIt (n - 1) x0 xss

ordered :: Int -> [Int] -> [[Int]] -> [Int]
ordered n (x0 : x0s) xss
	| (x : xs) <- xss !! n, x == x0 = x0 : ordered (n + 1) x0s xss'
	| fnd = x0 : ordered n x0s xss'
	| otherwise = ordered n x0s xss'
	where
	(fnd, xss') = takeIt (n + 1) x0 xss
