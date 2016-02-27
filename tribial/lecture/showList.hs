showL :: Show a => [a] -> ShowS
showL [] = ("[]" ++)
showL (x : xs) = ("[" ++) . shows x . sl xs
	where
	sl [] = ("]" ++)
	sl (y : ys) = ("," ++) . shows y . sl ys
