newtype State s a = State (s -> (a, s))

instance Functor (State s) where
	fmap f (State m) = State $ \s -> let (a, s') = m s in (f a, s')
