data MyT = MyT Int Bool Char

instance Show MyT where
	showsPrec d (MyT i b c) = showParen (d > 10) $
		showsPrec 11 i . showString " " .
		showsPrec 11 b . showString " " .
		showsPrec 11 c

infix 2 :-<
infix 5 :-|
infix 8 :->

data WeekOp = Int :-< Bool
data MiddleOp = WeekOp :-| StrongOp
data StrongOp = Char :-> Double

instance Show WeekOp where
	showsPrec d (i :-< b) = showParen (d > 2) $
		showsPrec 3 i . showString " :-< " .
		showsPrec 3 b

instance Show MiddleOp where
	showsPrec d (w :-| s) = showParen (d > 5) $
		showsPrec 6 w . showString " :-| " .
		showsPrec 6 s

instance Show StrongOp where
	showsPrec d (c :-> f) = showParen (d > 8) $
		showsPrec 9 c . showString " :-> " .
		showsPrec 9 f

data RecOp = RecOp :- Int | Empty

instance Show RecOp where
	showsPrec _ Empty = showString "Empty"
	showsPrec d (r :- i) = showParen (d > 9) $
		showsPrec 9 r . showString " :- " .
		showsPrec 10 i
