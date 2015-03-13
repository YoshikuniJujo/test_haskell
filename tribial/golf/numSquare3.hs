import Control.Arrow
import Control.Applicative

main :: IO ()
main = interact $ (++ "\n") . show . checkN . read . head . lines

checkN :: Int -> Int
checkN n
	| n `oelem` squares = 1
	| n `oelem` squares2 = 2
	| n `oelem` squares3 = 3
	| otherwise = 4

oelem :: (Eq a, Ord a) => a -> [a] -> Bool
x `oelem` (x0 : xs)
	| x == x0 = True
	| x < x0 = False
	| otherwise = x `oelem` xs

squares, squares2, squares3 :: [Int]
squares = map (^ 2) [1 ..]
squares2 = ordered 0 [2 ..] adds
-- squares3 = ordered 0 (filter (not . (`oelem` squares2)) [3 ..]) adds2
squares3 = ordered 0 [3 ..] adds2

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
