{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- import Control.Monad.Cont

data VE r = V r

newtype Cont a = Cont { runCont :: forall r . ((a -> VE r) -> VE r) }

instance Functor Cont where
	fmap = (=<<) . (return .)

instance Applicative Cont where
	pure = return
	mf <*> mx = do f <- mf; x <- mx; return $ f x

instance Monad Cont where
	return a = Cont ($ a)
	(Cont c) >>= f = Cont $ \k -> c (\a -> runCont (f a) k)

-- some :: Some Int
-- some = return 888

other :: Cont Int
other = return 888
