{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

newtype Cont a = Cont { runCont :: forall r . (a -> r) -> r }

instance Functor Cont where
	fmap = (=<<) . (return .)

instance Applicative Cont where
	pure = return
	mf <*> mx = do f <- mf; x <- mx; return $ f x

instance Monad Cont where
	return a = Cont ($ a)
	Cont c >>= f = Cont $ \k -> c (\a -> runCont (f a) k)

callCC :: ((a -> Cont b) -> Cont a) -> Cont a
callCC f = Cont $ \k -> runCont (f (\a -> Cont $ \_ -> k a)) k
