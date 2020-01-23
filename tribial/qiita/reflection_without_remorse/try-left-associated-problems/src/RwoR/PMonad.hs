{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module RwoR.PMonad where

import RwoR.FTCQueue

class PMonad m where
	pure' :: a -> m a
	(>>=.) :: m a -> FTCQueue m a b -> m b

val :: PMonad m => FTCQueue m a b -> (a -> m b)
val q = case tviewl q of
	TOne f -> f
	h :| t -> \x -> h x >>=. t

expr :: (a -> m b) -> FTCQueue m a b
expr = tsingleton

instance PMonad m => Functor m where
	f `fmap` m = pure . f =<< m

instance PMonad m => Applicative m where
	pure = pure'
	mf <*> mx = mf >>= \f -> mx >>= \x -> pure $ f x

instance PMonad m => Monad m where
	m >>= f = m >>=. expr f
