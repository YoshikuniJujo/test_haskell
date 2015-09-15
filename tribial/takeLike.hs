
npleRaw, npleZ, npleF :: [Integer] -> Integer -> [Integer]
npleRaw (x : xs) n = x * n : npleRaw xs (n + 1)
npleRaw _ _ = []

npleZ xs n = zipWith (*) xs [n ..]

npleF = foldr (\x f n -> x * n : f (n + 1)) (const [])
