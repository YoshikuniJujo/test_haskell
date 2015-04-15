main :: IO ()
main = interact $ (++"\n") . show . (\[p, q, e] -> getD p q e) . map read . take 3 . lines

getD :: Integer -> Integer -> Integer -> Integer
getD p q e = let d = snd . snd $ gcd' 0 1 1 0 ((p - 1) * (q - 1)) e in
	if d < 0 then (p - 1) * (q - 1) + d else d

gcd' :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> (Integer, (Integer, Integer))
gcd' a a' b b' m 0 = (m, (a', b'))
gcd' a a' b b' m n = gcd' (a' - q * a) a (b' - q * b) b n r
	where
	(q, r) = m `divMod` n
