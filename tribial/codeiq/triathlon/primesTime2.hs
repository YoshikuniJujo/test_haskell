import Data.List
import Data.Time

fs :: [Integer]
fs@(_ : t) = 3 : 0 : 2 : zipWith (+) t fs

ps :: [Integer]
ps = [ f | i <- [1 ..], let f = fs !! fromIntegral i, f `mod` i == 0 ]

hs :: [Integer]
hs = map ((primes !!) . (subtract 1)) primes

h :: Integer -> Integer
h n = sum $ takeWhile (<= n) hs

factors :: Integer -> [Integer]
factors = unfoldr uncons

uncons :: Integer -> Maybe (Integer, Integer)
uncons n
	| n < 2 = Nothing
	| otherwise = Just (f, r)
	where
	(f, r) : _ = [ (p, d) | p <- primes, let (d, m) = n `divMod` p, m == 0 ]

main :: IO ()
main = do
	t0 <- getCurrentTime
	let	i = h 120739
	print i
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
