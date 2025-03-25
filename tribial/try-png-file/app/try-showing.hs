{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TypeApplications, ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Except
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Pipe
import Data.Pipe.ByteString
import Data.ByteString qualified as BS
import System.Environment

import Chunks.SomeChunk
import Chunks.PipeNew
import Chunks.Core
import Chunks.MagicAndEnd

import Control.MonadClasses.State qualified as MC
import Control.MonadClasses.Except qualified as MC

newtype MyMonad a = MyMonad (
	ExceptT String (
	StateT (Maybe Ihdr) (
	StateT () (
	StateT BS.ByteString (
	IO)))) a)
	deriving (Functor, Applicative, Monad, MonadBase IO, MonadBaseControl IO)

runMyMonad :: MyMonad a -> IO (((Either String a, Maybe Ihdr), ()), BS.ByteString)
runMyMonad (MyMonad m) = (`runStateT` "") . (`runStateT` ()) . (`runStateT` Nothing) $ runExceptT m

instance MC.MonadState BS.ByteString MyMonad where
	get = MyMonad . lift . lift $ lift get
	put = MyMonad . lift . lift . lift . put

instance MC.MonadState () MyMonad where
	get = MyMonad . lift $ lift get
	put = MyMonad . lift . lift . put

instance MC.MonadState (Maybe Ihdr) MyMonad where
	get = MyMonad get
	put = MyMonad . put

instance MC.MonadError String MyMonad where
	throwError = MyMonad . throwError
	catchError (MyMonad m) h = MyMonad $ catchError m (\e -> let MyMonad m' = h e in m')

main :: IO ()
main = do
	fp : _ <- getArgs
	(print . fst . fst =<<) . runMyMonad . runPipe $ fromFile @Pipe fp
		=$= chunks [type Ihdr, type Idat, type Iend]
		=$= fix \go -> do
			mc <- await
			case mc of
				Just c -> do
					yield c
					case fromChunk c of
						Just i -> MC.put @(Maybe Ihdr) (Just i) >> go
						Nothing -> go
				Nothing -> pure ()
		=$= printAll 18

printAll :: (Show a, MonadBase IO m) => Int -> Pipe a b m ()
printAll 0 = pure ()
printAll n = do
	mx <- await
	case mx of
		Just x -> lift (liftBase . putStrLn . take 4000 $ show x) >> printAll (n - 1)
		Nothing -> pure ()
