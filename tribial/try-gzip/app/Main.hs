{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.ByteString qualified as BS
import System.Environment

import Gzip
import MyMonadNew
import MonadByteString
import BitArray

import Control.Monad.Base
import Control.MonadClasses.State qualified as MC
import Control.MonadClasses.Except qualified as MC

main :: IO ()
main = do
	fp : _ <- getArgs
	cnt <- BS.readFile fp
	putStrLn . take 200 . show =<< runMyMonad (bsToBitArray cnt) tryReadGzip

tryReadGzip :: (
	MC.MonadError String m,
	MonadBase IO m, MC.MonadState BS.ByteString m ) => m ()
tryReadGzip = do
		ids <- takeBytes 2
		print' $ ids == ids0
		cm <- pop
		fs <- maybe (MC.throwError @String "bad flags") pure . readFlags =<< pop
		mt <- takeWord32
		efs <- pop
		os <- pop
		fn <- takeString
		print' $ GzipHeader {
			gzipHeaderCompressionMethod = cm,
			gzipHeaderFlags = fs,
			gzipHeaderModificationTime = mt,
			gzipExtraFlags = efs,
			gzipOperatingSystem = os,
			gzipFileName = fn }
		print' . bits =<< pop
		print' . bits =<< pop
