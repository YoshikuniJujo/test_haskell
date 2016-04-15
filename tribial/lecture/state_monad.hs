{-# LANGUAGE MonadComprehensions #-}

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
	fmap = (=<<) . (return .)

instance Applicative (State s) where
	pure = return
	mf <*> mx = [ f x | f <- mf, x <- mx ]

instance Monad (State s) where
	State m >>= f = State $ \s -> let (x, s') = m s in runState (f x) s'
	return = State . (,)
