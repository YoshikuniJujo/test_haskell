import Data.List

m :: Int
m = 999

path = [
	[  m, 30,  m, 20,120],
	[  m,  m, 70,  m,  m],
	[  m,  m,  m,  m, 30],
	[  m,  m, 50,  m, 90],
	[  m,  m,  m,  m,  m]
 ]

next :: Int -> [Int] -> [[Int]] -> [Int]
next i d p = map (\j -> min (d !! j) (d !! i + p !! i !! j)) [0 .. 4]

minElem :: (Ord a, Bounded a) => [Int] -> [a] -> Int
minElem = minElemI (- 1) maxBound
	where
	minElemI :: Ord a => Int -> a -> [Int] -> [a] -> Int
	minElemI mi _ [] _ = mi
	minElemI mi mx (i : is) xs
		| mx > xs !! i = minElemI i (xs !! i) is xs
		| otherwise = minElemI mi mx is xs

run :: [Int] -> [Int] -> [[Int]] -> [Int]
run [] d p = d
run is d p = run (mi `delete` is) (next mi d p) p
	where
	mi = minElem is d

r :: [[Int]] -> [Int]
r p = run [1 .. length p - 1] (head p) p
