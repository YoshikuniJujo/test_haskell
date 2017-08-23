primes :: [Word]
primes = 2 : filter isPrimes [3 ..]

isPrimes :: Word -> Bool
isPrimes n | n < 2 = False
isPrimes n = all ((/= 0) . mod n) $ takeWhile ((<= n) . (^ 2)) primes
