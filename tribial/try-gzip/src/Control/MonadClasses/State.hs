{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.MonadClasses.State where

class Monad m => MonadState s m where get :: m s; put :: s -> m ()

gets :: MonadState s m => (s -> t) -> m t
gets f = f <$> get

modify :: MonadState s m => (s -> s) -> m ()
modify f = put . f =<< get
