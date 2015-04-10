zorome :: Int -> [Integer]
zorome n = map (sum . take n . iterate (* 10)) [1 .. 9]

nums :: Int -> [Integer]
nums n = [h * 10 ^ n + z | h <- [0 ..], z <- zorome n]

check :: Int -> Integer -> Bool
check n i = (i + 1) ^ 2 `div` 10 ^ (2 * n) - i ^ 2 `div` 10 ^ (2 * n) > 0
	&& (i + 1) ^ 2 `mod` 10 ^ (2 * n) /= 0

toAnswer :: Int -> Integer -> Integer
toAnswer n = (+ 1) . (`div` 10 ^ (2 * n)) . (^ 2)

getAnswer :: Int -> Integer
getAnswer n = toAnswer n . head . filter (check n) $ nums n

main :: IO ()
main = print $ getAnswer 6
