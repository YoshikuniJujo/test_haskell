{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.MonadClasses.Except where

class Monad m => MonadError e m where
	throwError :: e -> m a
	catchError :: m a -> (e -> m a) -> m a

liftEither :: MonadError e m => Either e a -> m a
liftEither = either throwError pure

tryError :: MonadError e m => m a -> m (Either e a)
tryError a = (Right <$> a) `catchError` (pure . Left)

-- ...
