{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.MonadClass where

import Control.Monad.Trans
import Control.Monad.State qualified as TF

class Monad m => MonadState s m where get :: m s; put :: s -> m ()

modify :: MonadState s m => (s -> s) -> m ()
modify f = put . f =<< get

instance MonadState Int MyMonad where
	get = TF.get
	put = TF.put

instance MonadState String MyMonad where
	get = lift TF.get
	put = lift . TF.put

type MyMonad = TF.StateT Int (TF.StateT String IO)

mySample :: MyMonad ()
mySample = do
	put (8 :: Int)
	put "Hello"
	modify (+ (15 :: Int))
	modify (++ ", world!")
	get >>= liftIO . print @Int
	get >>= liftIO . print @String
