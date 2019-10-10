tpq :: Integral n => n -> n -> (n, n) -> (n, n)
tpq p q (a, b) = (p * a + q * b, q * a + (p + q) * b)

tpqn :: Integral n => n -> n -> Int -> (n, n) -> (n, n)
tpqn p q n
	| n < 1 = id
	| even n = tpqn (p ^ 2 + q ^ 2) (2 * p * q + q ^ 2) (n `div` 2)
	| otherwise = tpqn p q (n - 1) . tpq p q

fastFib :: Integral n => Int -> n
fastFib n = fst $ tpqn 0 1 n (0, 1)

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
