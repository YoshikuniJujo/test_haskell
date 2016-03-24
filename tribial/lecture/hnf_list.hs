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

myHead :: [a] -> Id a
myHead xs = value xs Head

some = (:) 1 some

-- myHead some
-- myHead (value some)
-- (value some) Head
-- 1
