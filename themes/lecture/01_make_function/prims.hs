isPrim n = all (/= 0) $ map (n `mod`) $ takeWhile ((<= n) . (^ 2)) primes

primes = 2 : filter isPrim [3 .. ]
