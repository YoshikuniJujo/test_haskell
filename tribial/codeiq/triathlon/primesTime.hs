import Data.List
import Data.Time

main :: IO ()
main = do
	t0 <- getCurrentTime
	print $ primes !! 11362
	t1 <- getCurrentTime
	print $ t1 `diffUTCTime` t0

---------------------------------------------------------------------------
-- LAZY WHEEL SIEVES
--
-- Reference
-- Runciman, C. (1997) FUNCTIONAL PEARL: lazy wheel sieves and spirals of primes.
-- Journal of Functional Programming. pp. 219-225. ISSN 1469-7653
--
-- http://eprints.whiterose.ac.uk/3784/

data Wheel n = Wheel n [n] [n] deriving Show

wheels :: Integral n => [Wheel n]
wheels = Wheel 1 [1] [] : zipWith3 next wheels primes squares

next :: Integral n => Wheel n -> n -> n -> Wheel n
next (Wheel s ms ns) p q = Wheel (s * p) (modp ms ++ ms') ns'
	where
	(ms', ns') = span (<= q) . modp $ ns ++
		[ o + n | o <- [s, 2 * s .. (p - 1) * s], n <- ms ++ ns ]
	modp = filter ((> 0) . (`mod` p))

primes, squares :: Integral n => [n]
primes = spiral wheels squares
squares = (^ (2 :: Int)) `map` primes

spiral :: Integral n => [Wheel n] -> [n] -> [n]
spiral (Wheel s ms ns : ws) ~(q : qs) = foldr (turn 0) (roll s) ns
	where
	roll o = foldr (turn o) (roll $ o + s) $ ms ++ ns
	turn 1 1 rs = 2 : rs
	turn 3 1 _ = tail $ spiral ws qs
	turn 8 1 _ = tail $ spiral ws qs
	turn o n rs = if o + n < q then o + n : rs else spiral ws qs
spiral _ _ = error "never occur"
