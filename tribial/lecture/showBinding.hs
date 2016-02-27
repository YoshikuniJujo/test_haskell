data A = B :+: B

data B = Int :*: Int

infixl 6 :+:
infixl 7 :*:

{-
instance Show A where
	showsPrec d (b :+: c)
		| d > 6 = ("(" ++)
			. showsPrec 7 b . (" :+: " ++) . showsPrec 7 c
			. (")" ++)
		| otherwise = showsPrec 7 b . (" :+: " ++) . showsPrec 7 c

instance Show B where
	showsPrec d (m :*: n)
		| d > 7 = ("(" ++)
			. showsPrec 8 m . (" :*: " ++) . showsPrec 8 n
			. (")" ++)
		| otherwise = showsPrec 8 m . (" :*: " ++) . showsPrec 8 n
		-}

instance Show A where
	showsPrec d (b :+: c) = showParen (d > 6) $
		showsPrec 7 b . (" :+: " ++) . showsPrec 7 c

instance Show B where
	showsPrec d (m :*: n) = showParen (d > 7) $
		showsPrec 8 m . (" :*: " ++) . showsPrec 8 n
