main :: IO ()
main = interact $ (++ "\n") . show . checkSquares . read . head . lines

squares :: [Int]
squares = map (^ 2) [1 ..]

twos :: [Int]
twos = filter checkTwo [1 ..]

checkTwo :: Int -> Bool
checkTwo n | any (`elem` ss) ds = True | otherwise = False
	where
	ss = takeWhile (<= n) squares
	ds = map (n -) ss

checkSquares :: Int -> Int
checkSquares n
	| any (== 0) ds = 1
	| any (`elem` ss) ds = 2
	| any (`elem` ts) ds = 3
	| otherwise = 4
	where
	ss = takeWhile (<= n) squares
	ds = map (n -) ss
	ts = takeWhile (<= n) twos
