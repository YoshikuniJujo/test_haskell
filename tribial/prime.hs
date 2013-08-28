import Data.List

primes :: [Integer]
primes = concatMap head primesLists

primesLists :: [[[Integer]]]
primesLists = mapIterate sieve primes nums

sieve :: Integer -> [[Integer]] -> [[Integer]]
sieve p = map (filter $ (/= 0) . (`mod` p)) . tail

nums :: [[Integer]]
nums = [2, 3] : separate [4 .. ] (zipWith (-) (tail p2) p2)
	where p2 = map (^2) primes

--------------------------------------------------------------------------------

separate :: [b] -> [Integer] -> [[b]]
separate xs (n : ns) = genericTake n xs : separate (genericDrop n xs) ns

mapIterate :: (a -> b -> b) -> [a] -> b -> [b]
mapIterate op ~(x : xs) y0 = y0 : mapIterate op xs (op x y0)

primes' = [ p | (p : _) <- iterate sieve' [2..] ]
	where sieve' (p : ps) = [x | x <- ps, mod x p /= 0]
