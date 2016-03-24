data Some = Some Int Int Char deriving Show

data Selector = Fst | Snd | Trd deriving Show

some :: Value a => Selector -> a
some = value (Some 15 21 'a')

class Value a where
	value :: Some -> Selector -> a

instance Value Int where
	value (Some f _ _) Fst = f
	value (Some _ s _) Snd = s
	value _ _ = error "type mismatch"

instance Value Char where
	value (Some _ _ t) Trd = t
	value _ _ = error "type mismatch"
