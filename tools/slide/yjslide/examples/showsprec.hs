infixl 6 :+:
infixl 7 :*:

data Calc
	= N Int
	| Calc :+: Calc
	| Calc :*: Calc

instance Show Calc where
	showsPrec d (N n) = showParen (d > 10) $
		showString "N " . showsPrec 11 n
	showsPrec d (l :+: r) = showParen (d > 6) $
		showsPrec 7 l . showString " :+: " . showsPrec 7 r
	showsPrec d (l :*: r) = showParen (d > 7) $
		showsPrec 8 l . showString " :*: " . showsPrec 8 r

showList' :: Show a => [a] -> ShowS
showList' [] s = "[]" ++ s
showList' (x : xs) s = '[' : shows x (showl xs)
	where
	showl [] = ']' : s
	showl (y : ys) = ',' : shows y (showl ys)
