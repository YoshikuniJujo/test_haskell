{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import "monads-tf" Control.Monad.State
import Control.Monad.Base
import Data.Pipe
import Data.Pipe.ByteString
import Data.ByteString qualified as BS
import System.IO
import System.Environment

import Chunks.Pipe

main :: IO ()
main = do
	fp : _ <- getArgs
	png <- BS.readFile fp
	(`evalStateT` "") . runPipe $
		fromFile @Pipe fp =$= (takeByteString 8 >> chunk' []) =$= printAll 10
	pure ()

printAll :: (Show a, MonadBase IO m) => Int -> Pipe a b m ()
printAll 0 = pure ()
printAll n = do
	mx <- await
	case mx of
		Just x -> lift (liftBase . putStrLn . take 100 $ show x) >> printAll (n - 1)
		Nothing -> pure ()
