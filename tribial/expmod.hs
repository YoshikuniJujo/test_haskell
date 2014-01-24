import System.Random

expmod :: Integer -> Integer -> Integer -> Integer
expmod _ 0 _ = 1
expmod base exp m
	| even exp = square (expmod base (exp `div` 2) m) `rem` m
	| otherwise = expmod base (exp - 1) m * base `rem` m

square :: Integer -> Integer
square = (^ 2)

tryIt :: Integer -> Integer -> Bool
tryIt n a = expmod a n n == a

fermatTest :: Integer -> IO Bool
fermatTest n = do
	a <- randomRIO (1, n - 1)
	return $ tryIt n a

fastIsPrime :: Integer -> Int -> IO Bool
fastIsPrime n 0 = return True
fastIsPrime n times = do
	isP <- fermatTest n
	if isP then fastIsPrime n (times - 1) else return False
