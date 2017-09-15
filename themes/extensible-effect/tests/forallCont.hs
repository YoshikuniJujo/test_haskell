{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- import Control.Monad.Cont

data VE s r = V r | S (State s (VE s r))

data State s a = State (s -> s) (s -> a)

modify :: (s -> s) -> Cont s s
modify f = Cont $ S . State f

{-
runState :: Cont (VE s s) a -> s -> a
runState m = sloop (runCont m V)

sloop :: VE s s -> a
sloop m s = case m of
	V x -> x
	S (State f k) -> sloop (k s) (f s)
	-}

newtype Cont s a = Cont { runCont :: forall r . ((a -> VE s r) -> VE s r) }

instance Functor (Cont s) where
	fmap = (=<<) . (return .)

instance Applicative (Cont s) where
	pure = return
	mf <*> mx = do f <- mf; x <- mx; return $ f x

instance Monad (Cont s) where
	return a = Cont ($ a)
	(Cont c) >>= f = Cont $ \k -> c (\a -> runCont (f a) k)

-- some :: Some Int
-- some = return 888

other :: Cont s Int
other = return 888
