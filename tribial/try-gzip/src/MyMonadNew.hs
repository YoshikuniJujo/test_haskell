{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MyMonadNew where

import Control.Monad.Base
import Control.Monad.State
import Control.Monad.Except

import Control.MonadClasses.State qualified as MC
import Control.MonadClasses.Except qualified as MC

import Data.ByteString qualified as BS

import BitArray

newtype MyMonad a = MyMonad {
	unMyMonad :: StateT BitArray (ExceptT String IO) a }
	deriving (Functor, Applicative, Monad, MonadIO)

instance MC.MonadError String MyMonad where
	throwError = MyMonad . throwError
	catchError x = MyMonad . catchError (unMyMonad x) . (unMyMonad .)

instance MC.MonadState BS.ByteString MyMonad where
	get = MyMonad do
		ba <- get
		either throwError pure $ bitArrayToBs ba
	put = MyMonad . put . bsToBitArray

instance MonadBase IO MyMonad where liftBase = MyMonad . liftBase

runMyMonad ::
	BitArray -> MyMonad a -> IO (Either String (a, BitArray))
runMyMonad bs = runExceptT . (`runStateT` bs) . unMyMonad
