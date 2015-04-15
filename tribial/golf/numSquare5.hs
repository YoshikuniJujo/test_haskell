main :: IO ()
main = interact $ (++ "\n") . show . checkSquares . read . head . lines

squares :: [Int]
squares = map (^ 2) [1 ..]

twos :: [Int]
twos = filter checkTwo [1 ..]

checkTwo :: Int -> Bool
checkTwo n | any' (`oelem` squares) ds = True | otherwise = False
	where
	ds = map (n -) squares

checkSquares :: Int -> Int
checkSquares n
	| any' (`oelem` [0]) ds = 1
	| any' (`oelem` squares) ds = 2
	| any' (`oelem` twos) ds = 3
	| otherwise = 4
	where
	ds = map (n -) squares

any' :: (a -> Maybe Bool) -> [a] -> Bool
any' p (x : xs)
	| Just True <- p x = True
	| Nothing <- p x = False
	| otherwise = any' p xs

oelem :: (Num a, Eq a, Ord a) => a -> [a] -> Maybe Bool
oelem _ [] = Just False
oelem x (x0 : xs)
	| x < 0 = Nothing
	| x == x0 = Just True
	| x < x0 = Just False
	| otherwise = oelem x xs
