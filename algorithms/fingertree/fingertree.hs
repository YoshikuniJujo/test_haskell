data FingerTree a
	= Empty
	| Single a
	| Deep Int (Digit a) (FingerTree (Node a)) (Digit a)
	deriving Show

data Digit a
	= One a
	| Two a a
	| Three a a a
	| Four a a a a
	deriving Show

data Node a
	= Node2 Int a a
	| Node3 Int a a a
	deriving Show

empty :: FingerTree a
empty = Empty

class Sized a where
	size :: a -> Int

instance Sized (Node a) where
	size (Node2 v _ _) = v
	size (Node3 v _ _ _) = v

(<|) :: Sized a => a -> FingerTree a -> FingerTree a
a <| Empty = Single a
a <| Single b = Deep 2 (One a) Empty (One b)
a <| Deep s (One b) m sf = Deep (size a + s) (Two a b) m sf
a <| Deep s (Two b c) m sf = Deep (size a + s) (Three a b c) m sf
a <| Deep s (Three b c d) m sf = Deep (size a + s) (Four a b c d) m sf
a <| Deep s (Four b c d e) m sf = m `seq`
	Deep (size a + s) (Two a b) (node3 c d e <| m) sf

node3 :: Sized a => a -> a -> a -> Node a
node3 a b c = Node3 (size a + size b + size c) a b c
