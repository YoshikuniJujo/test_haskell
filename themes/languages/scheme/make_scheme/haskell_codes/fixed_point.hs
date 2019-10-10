tolerance :: Fractional f => f
tolerance = 0.00001

fixedPoint :: (Ord f, Fractional f) => (f -> f) -> f -> f
fixedPoint f x
	| abs (x' - x) < tolerance = x'
	| otherwise = fixedPoint f x'
	where
	x' = f x

average :: Fractional f => f -> f -> f
average x y = (x + y) / 2

mySqrt :: (Ord f, Fractional f) => f -> f
mySqrt x = fixedPoint (\y -> average y (x / y)) 1
