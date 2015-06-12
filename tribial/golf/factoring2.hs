main :: IO ()
main = interact $ (\(a, b, c, d, e) -> unwords $ map show [a, b, c, d, e])
	. (\[p, q, r] -> factoring p q r) . map read . words
	
factoring :: Int -> Int -> Int -> (Int, Int, Int, Int, Int)
factoring p q r = (p `div` bb `div` dd, bb, cc, dd, ee)
	where
	[p_, q_, r_] = fromIntegral `map` [p, q, r]
	z = round $ (q_ * q_ - 4 * p_ * r_) ** 0.5
	[b, c, e] = [2 * p, q + z, q - z]
	[g, h] = [gcd c b, gcd e b]
	[b', c', d', e'] = [b `div` g, c `div` g, b `div` h, e `div` h]
	[bb, cc, dd, ee] = if (d', e') > (b', c')
		then [d', e', b', c']
		else [b', c', d', e']
