{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.MonadClasses.Except where

import Control.Exception (IOException, catch, ioError)

class Monad m => MonadError e m where
	throwError :: e -> m a; catchError :: m a -> (e -> m a) -> m a

instance MonadError IOException IO where
	throwError = ioError; catchError = catch

liftEither :: MonadError e m => Either e a -> m a
liftEither = either throwError pure

instance MonadError () Maybe where
	throwError () = Nothing; catchError = maybe ($ ()) (const . Just)

instance MonadError e (Either e) where
	throwError = Left
	catchError = either (flip ($)) (const . Right)
