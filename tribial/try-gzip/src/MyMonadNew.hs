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

import Data.Pipe
import Data.ByteString qualified as BS

import BitArray

import HuffmanTree

type MyPipe i o = Pipe i o MyMonad

runMyPipe ::
	(BinTree Int, BinTree Int) ->
	BitArray -> MyPipe i o a -> IO (Either String ((Maybe a, BitArray), (BinTree Int, BinTree Int)))
runMyPipe ht bs = runExceptT . (`runStateT` ht) . (`runStateT` bs) . unMyMonad . runPipe

newtype MyMonad a = MyMonad {
	unMyMonad :: StateT BitArray
		(StateT (BinTree Int, BinTree Int)
		(ExceptT String IO)) a }
	deriving (Functor, Applicative, Monad, MonadIO)

instance MC.MonadError String MyMonad where
	throwError = MyMonad . throwError
	catchError x = MyMonad . catchError (unMyMonad x) . (unMyMonad .)

instance MonadFail MyMonad where
	fail = MC.throwError

instance MC.MonadState BS.ByteString MyMonad where
	get = MyMonad do
		ba <- get
		either throwError pure $ bitArrayToBs ba
	put = MyMonad . put . bsToBitArray

instance MC.MonadState BitArray MyMonad where
	get = MyMonad get
	put = MyMonad . put

instance MC.MonadState (BinTree Int, BinTree Int) MyMonad where
	get = MyMonad $ lift get
	put = MyMonad . lift . put

instance MonadBase IO MyMonad where liftBase = MyMonad . liftBase

runMyMonad ::
	(BinTree Int, BinTree Int) ->
	BitArray -> MyMonad a -> IO (Either String ((a, BitArray), (BinTree Int, BinTree Int)))
runMyMonad ht bs = runExceptT . (`runStateT` ht) . (`runStateT` bs) . unMyMonad
