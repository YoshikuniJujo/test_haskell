{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.MonadClasses.Writer where

class (Functor m, Monoid w) => MonadWriter w m where
	{-# MINIMAL (writer | tell), listen, pass #-}

	writer :: (a, w) -> m a
	writer ~(a, w) = a <$ tell w

	tell :: w -> m ()
	tell w = writer ((), w)

	listen :: m a -> m (a, w)
	pass :: m (a, w -> w) -> m a
