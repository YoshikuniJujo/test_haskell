{-# LANGUAGE RankNTypes, MonadComprehensions #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ContinuationPassingStyle where

import PerformanceProblems

type DiffList a = [a] -> [a]

abs :: DiffList a -> [a]
abs = ($ [])

rep :: [a] -> DiffList a
rep = (.++)

(.++.) :: DiffList a -> DiffList a -> DiffList a
(.++.) = (.)

newtype DiffMonoid a = DiffMonoid { appDiffMonoid :: a -> a }

absMonoid :: Monoid a => DiffMonoid a -> a
absMonoid a = a `appDiffMonoid` mempty

repMonoid :: Monoid a => a -> DiffMonoid a
repMonoid = DiffMonoid . mappend

instance Monoid a => Semigroup (DiffMonoid a) where
	DiffMonoid a <> DiffMonoid b = DiffMonoid $ a . b

instance Monoid a => Monoid (DiffMonoid a) where
	mempty = repMonoid mempty
	DiffMonoid a `mappend` DiffMonoid b = DiffMonoid $ a . b

newtype CodensityT m a =
	CodensityT { runCodensityT :: forall b . (a -> m b) -> m b }

absM :: Monad m => CodensityT m a -> m a
absM = (`runCodensityT` return)

repM :: Monad m => m a -> CodensityT m a
repM ma = CodensityT (ma >>=)

instance Monad m => Functor (CodensityT m) where
	f `fmap` mx = pure f <*> mx

instance Monad m => Applicative (CodensityT m) where
	pure = repM . return
	mf <*> mx = [ f x | f <- mf, x <- mx ]

instance Monad m => Monad (CodensityT m) where
	CodensityT m >>= f = CodensityT $ \c -> m $ (`runCodensityT` c) . f
