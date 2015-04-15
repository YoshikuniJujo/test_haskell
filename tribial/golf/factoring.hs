main :: IO ()
main = interact $ (\(a, b, c, d, e) -> unwords $ map show [a, b, c, d, e])
	. (\[p, q, r] -> factoring p q r) . map read . words

factoring :: Int -> Int -> Int -> (Int, Int, Int, Int, Int)
factoring p_ q_ r_ = (signum p_ * a, b, c, d, e)
	where
	a = p_ `gcd` q_ `gcd` r_
	[p, q, r] = map ((* signum p_) . (`div` a)) [p_, q_, r_]
	z = q ^ 2 - 4 * p * r
	(b, c, d, e) : _ = [ (b, c, d, e) |
		b <- [abs p, abs p - 1 .. - abs p],
		c <- [abs r, abs r - 1 .. - abs r],
		d <- [abs p, abs p - 1 .. - abs p],
		e <- [abs r, abs r - 1 .. - abs r],
		b * d == p, b * e + c * d == q, c * e == r ]
