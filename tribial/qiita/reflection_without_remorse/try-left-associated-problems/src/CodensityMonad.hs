{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CodensityMonad (CodensityT, abs, rep) where

import Prelude hiding (abs)

newtype CodensityT m a = CodensityT {
	runCodensityT :: forall b . (a -> m b) -> m b }

abs :: Monad m => CodensityT m a -> m a
abs (CodensityT a) = a pure

rep :: Monad m => m a -> CodensityT m a
rep m = CodensityT (m >>=)

instance Monad m => Functor (CodensityT m) where
--	f `fmap` m = pure . f =<< m
--	f `fmap` CodensityT m = CodensityT \k -> m \x -> pure (f x) >>= k
	f `fmap` CodensityT m = CodensityT \k -> m \x -> k $ f x
	-- f `fmap` m = \k -> m \x -> k $ f x
	-- m = \k -> m \x -> k x

instance Monad m => Applicative (CodensityT m) where
	pure = rep . pure
	mf <*> mx = mf >>= \f -> mx >>= \x -> pure (f x)

instance Monad m => Monad (CodensityT m) where
	CodensityT m >>= f = CodensityT \k -> m \x -> runCodensityT (f x) k
