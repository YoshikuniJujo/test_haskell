{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications, ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Except
import Control.Monad.Base
import Data.Pipe
import Data.Pipe.ByteString
import System.Environment

import Chunks.Pipe
import Chunks.Core

main :: IO ()
main = do
	fp : _ <- getArgs
	(print . fst =<<) . (`runStateT` "") . runExceptT . runPipe $
		fromFile @Pipe fp =$= chunks [type Ihdr, type End] =$= printAll 15

printAll :: (Show a, MonadBase IO m) => Int -> Pipe a b m ()
printAll 0 = pure ()
printAll n = do
	mx <- await
	case mx of
		Just x -> lift (liftBase . putStrLn . take 200 $ show x) >> printAll (n - 1)
		Nothing -> pure ()
