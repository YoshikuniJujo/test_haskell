tpq :: Integral n => (n, n) -> (n, n) -> (n, n)
tpq (p, q) (a, b) = (p * a + q * b, q * a + (p + q) * b)

fastFib :: Integral n => Int -> n
fastFib = fst . fastFibRec (0, 1)

fastFibRec :: Integral n => (n, n) -> Int -> (n, n)
fastFibRec _ n | n < 1 = (0, 1)
fastFibRec pq@(p, q) n
	| even n = fastFibRec (p ^ 2 + q ^ 2, 2 * p * q + q ^ 2) (n `div` 2)
	| otherwise = tpq pq $ fastFibRec pq (n - 1)
