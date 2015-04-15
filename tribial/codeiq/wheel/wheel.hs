data Wheel = Wheel Int [Int] deriving Show

primes = sieve wheels primes squares

squares = (^ 2) `map` primes

sieve (Wheel s ns : ws) ps@(~(p : ps')) qs@(~(_ : qs')) = [ o + n |
		o <- s : [2 * s, 3 * s .. (p - 1) * s], n <- ns,
		s <= 2 || noFactorIn ps qs (o + n) ] ++
	sieve ws ps' qs'

wheels = Wheel 1 [1] : zipWith nextSize wheels primes

nextSize (Wheel s ns) p = Wheel (s * p)
	[ o + n | o <- [0, s .. (p - 1) * s], n <- ns, (o + n) `mod` p > 0 ]

noFactorIn (p : ps) (q : qs) x = q > x || x `mod` p > 0 && noFactorIn ps qs x

roll ns s p = roll' (p - 1) 0
	where
	roll' 0 _ = []
	roll' t o = foldr (turn o) (roll' (t - 1) (o + s)) ns
	turn o n xs = let n' = o + n in if c n' then n' : xs else xs
