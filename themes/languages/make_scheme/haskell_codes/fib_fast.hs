tpq :: Integral n => n -> n -> Int -> (n, n) -> (n, n)
tpq p q n
	| n < 1 = id
	| even n = tpq (p ^ 2 + q ^ 2) (2 * p * q + q ^ 2) (n `div` 2)
	| otherwise = (\(a, b) -> (a * (p + q) + b * q, a * q + b * p))
		. tpq p q (n - 1)

fastFib :: Integral n => Int -> n
fastFib n = snd $ tpq 0 1 n (1, 0)

fib :: Int -> Integer
fib = (fibs !!) . fromIntegral

fibs, tfibs :: [Integer]
fibs@(_ : tfibs) = 0 : 1 : zipWith (+) fibs tfibs

fibs' :: Integral n => [n]
fibs' = 0 : 1 : zipWith (+) fibs' (tail fibs')

fibs'' :: [Integer]
fibs'' = 0 : 1 : zipWith (+) fibs'' (tail fibs'')

main :: IO ()
main = do
	print $ take 32 fibs'
	print $ take 32 fibs''
