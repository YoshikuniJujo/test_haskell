{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- import Control.Monad.Cont

data VE r = V r

newtype Cont r a = Cont { runCont :: (a -> r) -> r }

newtype Cont' a = Cont' (forall r . Cont r a)

runCont' :: Cont' a -> a
runCont' (Cont' c) = -- let V x = runCont c V in x
	runCont c id

instance Functor Cont' where
	fmap = (=<<) . (return .)

instance Applicative Cont' where
	pure = return
	mf <*> mx = do f <- mf; x <- mx; return $ f x

instance Monad Cont' where
	return a = Cont' $ Cont ($ a)
	Cont' c >>= f = Cont' $ c >>= (\(Cont' d) -> d) . f

instance Functor (Cont r) where
	fmap = (=<<) . (return .)

instance Applicative (Cont r) where
	pure = return
	mf <*> mx = do f <- mf; x <- mx; return $ f x

instance Monad (Cont r) where
	return a = Cont ($ a)
	(Cont c) >>= f = Cont $ \k -> c (\a -> runCont (f a) k)

some :: Cont a Int
some = return 888

other :: Cont' Int
other = return 888
