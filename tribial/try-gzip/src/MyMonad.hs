{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MyMonad where

import Control.Monad.Trans
import Control.Monad.Base
import Control.Monad.State
import Control.Monad.Except
import Data.ByteString qualified as BS

import Control.MonadClasses.State qualified as MC
import Control.MonadClasses.Except qualified as MC

newtype MyMonad a = MyMonad {
	unMyMonad :: StateT BS.ByteString (ExceptT String IO) a }
	deriving (Functor, Applicative, Monad, MonadIO)

instance MC.MonadError String MyMonad where
	throwError = MyMonad . throwError
	catchError x = MyMonad . catchError (unMyMonad x) . (unMyMonad .)

instance MC.MonadState BS.ByteString MyMonad where
	get = MyMonad get; put = MyMonad . put

instance MonadBase IO MyMonad where liftBase = MyMonad . liftBase

runMyMonad ::
	BS.ByteString -> MyMonad a -> IO (Either String (a, BS.ByteString))
runMyMonad bs = runExceptT . (`runStateT` bs) . unMyMonad
