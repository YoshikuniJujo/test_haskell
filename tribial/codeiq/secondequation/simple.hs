import Data.List

main :: IO ()
main = interact $ (++ "\n") . show . (scanl1 (+) bs !!) . pred . read
-- main = interact $ (++ "\n") . show . result . read

count :: Integer -> Integer
count n = sum $ map (n `div`) [1 .. n]

bs :: [Integer]
bs = map (count . (`div` 4) . (^ 2)) [1 ..]

count' :: Integer -> Integer
count' 0 = 0
-- count' n = sum (map (fromIntegral . fromEnum . (== 0) . (n `mod`)) [1 .. n]) + count' (n - 1)
-- count' n = fromIntegral (length $ filter ((== 0) . (n `mod`)) [1 .. n]) + count' (n - 1)
count' n = factNum n + count' (n - 1)

result :: Integer -> Integer
result n = sum $ zipWith (*) [n - 1, n - 2 .. 1] nums

nums :: [Integer]
nums = map sum $ takes factNums ns'

factNums :: [Integer]
factNums = map factNum [1 ..]

ns :: [Int]
ns = map ((`div` 4) . (^ 2)) [1 ..]

ns' :: [Int]
ns' = zipWith (-) (tail ns) ns

takes :: [a] -> [Int] -> [[a]]
takes xs (i : is) = let (l, r) = splitAt i xs in l : takes r is

factNum :: Integer -> Integer
factNum = other . some . factorization

other :: [Integer] -> Integer
other = product . map (+ 1)

some :: [Integer] -> [Integer]
some = map (fromIntegral . length) . group

factorization :: Integer -> [Integer]
factorization = unfoldr uncons

uncons :: Integer -> Maybe (Integer, Integer)
uncons n | n < 2 = Nothing
uncons n = Just (f, n `div` f)
	where f = head $ filter ((== 0) . (n `mod`)) [2 ..]
