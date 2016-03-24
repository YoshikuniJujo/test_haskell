data Selector = Head | Tail deriving Show

newtype Id a = Id a deriving Show

class Value v where
	value :: [a] -> Selector -> v a

instance Value [] where
	value (_ : t) Tail = t
	value _ _ = error "type mismatch"

instance Value Id where
	value (h : _) Head = Id h
	value _ _ = error "type mismatch"
