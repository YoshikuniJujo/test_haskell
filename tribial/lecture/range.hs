data Range = Range Double Double deriving Show

instance Num Range where
	Range a b + Range c d = Range (a + c) (b + d)
	Range a b * Range c d = Range (minimum f) (maximum f)
		where f = [a * c, a * d, b * c, b * d]
	negate (Range a b) = Range (- b) (- a)
	abs (Range a b)
		| b < 0 = Range (- b) (- a)
		| a < 0 = Range 0 (max (- a) b)
		| otherwise = Range a b
	signum (Range a b) = Range (signum a) (signum b)
	fromInteger n = Range (fromInteger n - 0.5) (fromInteger n + 0.5)
