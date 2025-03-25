{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import System.Environment

import Control.Monad.Base
import Control.Monad.State
import Control.Monad.Except
import Control.MonadClasses.State qualified as MC
import Control.MonadClasses.Except qualified as MC
import Data.Pipe
import Data.Pipe.ByteString

import Png.Chunk.Pipe

instance (MonadState m, StateType m ~ s) => MC.MonadState s m where
	get = get; put = put

instance (MonadError m, ErrorType m ~ e) => MC.MonadError e m where
	throwError = throwError; catchError = catchError

main :: IO ()
main = do
	fp : _ <- getArgs
	(print =<<) . runExceptT . (`runStateT` "") . runPipe
		$ fromFile @Pipe fp
			=$= (checkMagic >> chunk >> chunk >> chunk)
			=$= printAll

printAll :: (Show a, MonadBase IO m) => Pipe a b m ()
printAll = do
	mx <- await
	case mx of
		Just x -> lift (liftBase $ print x) >> printAll
		Nothing -> pure ()
