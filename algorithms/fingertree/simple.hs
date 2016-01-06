data FingerTree a
	= Empty
	| Single a
	| Deep (Digit a) (FingerTree (Node a)) (Digit a)
	deriving Show

data Digit a
	= One a
	| Two a a
	| Three a a a
	| Four a a a a
	deriving Show

data Node a
	= Node2 a a
	| Node3 a a a
	deriving Show

(<|) :: a -> FingerTree a -> FingerTree a
a <| Empty = Single a
a <| Single b = Deep (One a) Empty (One b)
a <| Deep (One b) m sf = Deep (Two a b) m sf
a <| Deep (Two b c) m sf = Deep (Three a b c) m sf
a <| Deep (Three b c d) m sf = Deep (Four a b c d) m sf
a <| Deep (Four b c d e) m sf = Deep (Two a b) (Node3 c d e <| m) sf
