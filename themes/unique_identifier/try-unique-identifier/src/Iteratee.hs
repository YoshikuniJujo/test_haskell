{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module Iteratee where

import FTCQueue

data It i a = Done a | Get (MExp (It i) i a)

class PMonad m where
	return' :: a -> m a
	(>>=^) :: m a -> MExp m a b -> m b

instance {-# OVERLAPPABLE #-} PMonad m => Functor m where
	f `fmap` m = m >>=^ expr (return' . f)

instance {-# OVERLAPPABLE #-} PMonad m => Applicative m where
	pure = return'
	mf <*> mx = mf >>=^ expr (<$> mx)

instance {-# OVERLAPPABLE #-} PMonad m => Monad m where
	m >>= f = m >>=^ expr f

expr :: (a -> m b) -> MExp m a b
expr = tsingleton . MCont

val :: PMonad m => MExp m a b -> (a -> m b)
val q = case tviewl q of
	TOne (MCont f) -> f
	MCont h :| t -> \x -> h x >>=^ t

instance PMonad (It i) where
	return' = Done
	Done x >>=^ g = val g x
	Get f >>=^ g = Get (f >< g)

get :: It i i
get = Get $ expr pure

par :: It i a -> It i b -> It i (It i a, It i b)
par l r	| Get f <- l, Get g <- r = get >>= \x -> par (val f x) (val g x)
	| otherwise = pure (l, r)

apply :: It i a -> [i] -> Maybe a
Done x `apply` _ = Just x
_ `apply` [] = Nothing
Get f `apply` (i : is) = val f i `apply` is
