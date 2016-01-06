data FingerTree a
	= Empty | Single a
	| Deep (Digit a) (FingerTree (Node a)) (Digit a)
	deriving Show

data Digit a = One a | Two a a deriving Show

data Node a = Node2 a a | Node3 a a a deriving Show

(<|) :: a -> FingerTree a -> FingerTree a
a <| Empty = Single a
a <| Single b = Deep (One a) Empty (One b)
a <| Deep (One b) m c = Deep (Two a b) m c
a <| Deep (Two b c) m d = Deep (One a) (Node2 b c <| m) d
