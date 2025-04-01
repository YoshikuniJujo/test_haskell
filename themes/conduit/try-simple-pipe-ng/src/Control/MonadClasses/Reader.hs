{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.MonadClasses.Reader where

class Functor m => MonadReader r m where
	{-# MINIMAL (ask | reader), local #-}

	ask :: m r
	ask = reader id

	local :: (r -> r) -> m a -> m a

	reader :: (r -> a) -> m a
	reader f = f <$> ask
