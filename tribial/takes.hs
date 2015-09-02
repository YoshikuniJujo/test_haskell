import Data.List

takeRaw, takeF, takeU :: Int -> [a] -> [a]
takeRaw n (x : xs) | n > 0 = x : takeRaw (n - 1) xs
takeRaw _ _ = []

takeF = flip $ foldr s (const [])
	where s x f n | n > 0 = x : f (n - 1); s _ _ _ = []

takeU = curry . unfoldr $ \nl -> case nl of
	(n, x : xs) | n > 0 -> Just (x, (n - 1, xs))
	_ -> Nothing

dropRaw, dropF :: Int -> [a] -> [a]
dropRaw n (x : xs) | n > 0 = dropRaw (n - 1) xs
dropRaw _ xs = xs

dropF = flip $ foldr s (const [])
	where s _ f n | n > 0 = f $ n - 1; s x f _ = x : f 0

splitAtRaw, splitAtF :: Int -> [a] -> ([a], [a])
splitAtRaw n (x : xs) | n > 0 = (x : t, d) where (t, d) = splitAtRaw (n - 1) xs
splitAtRaw _ xs = ([], xs)

splitAtF = flip $ foldr s (const ([], []))
	where
	s x f n	| n > 0 = (x : t, d) where (t, d) = f (n - 1)
	s x f _ = ([], x : d) where (_, d) = f 0
