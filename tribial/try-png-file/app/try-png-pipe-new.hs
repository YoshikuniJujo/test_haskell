{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications, ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Except
import Control.Monad.Base
import Data.Pipe
import Data.Pipe.ByteString
import Data.ByteString qualified as BS
import System.Environment

import Chunks.PipeNew
import Chunks.Core

import Control.MonadClasses.State qualified as MC
import Control.MonadClasses.Except qualified as MC

instance (MonadState m, StateType m ~ s) => MC.MonadState s m where
	get = get; put = put

instance (MonadError m, ErrorType m ~ e) => MC.MonadError e m where
	throwError = throwError; catchError = catchError

main :: IO ()
main = do
	fp : _ <- getArgs
	(print . fst . fst =<<)
--	(print =<<)
		. (`runStateT` ())
		. (`runStateT` ("" :: BS.ByteString)) . runExceptT . runPipe
		$ fromFile @Pipe fp
			=$= chunks [type Ihdr, type Idat, type Iend]
			=$= printAll 18

printAll :: (Show a, MonadBase IO m) => Int -> Pipe a b m ()
printAll 0 = pure ()
printAll n = do
	mx <- await
	case mx of
		Just x -> lift (liftBase . putStrLn . take 200 $ show x) >> printAll (n - 1)
		Nothing -> pure ()
