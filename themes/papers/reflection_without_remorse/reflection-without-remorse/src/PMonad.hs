{-# LANGUAGE GADTs, FlexibleInstances, UndecidableInstances, MonadComprehensions #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module PMonad where

import TypeAlignedSequences
import TSequence

type TCQueue = Queue

newtype MCont m a b = MCont { appMCont :: a -> m b }
type MCExp m a b = TCQueue (MCont m) a b
type MExp m a = MCExp m () a

class PMonad m where
	return' :: a -> m a
	(>>=.) :: m a -> MCExp m a b -> m b

val :: PMonad m => MCExp m a b -> (a -> m b)
val q = case tviewl q of
	TEmptyL -> return'
	h :< t -> \x -> h `appMCont` x >>=. t

expr :: (a -> m b) -> MCExp m a b
expr = tsingleton . MCont

instance PMonad m => Functor m where
	f `fmap` mx = pure f <*> mx

instance PMonad m => Applicative m where
	pure = return'
	mf <*> mx = [ f x | f <- mf, x <- mx ]

instance PMonad m => Monad m where
	m >>= f = m >>=. expr f
