{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad.Except
import Data.ByteString qualified as BS
import System.Environment

import Gzip

sample0 :: FilePath
sample0 = "samples/abcd.txt.gz"

main :: IO ()
main = do
	fp : _ <- getArgs
	cnt <- BS.readFile fp
	print =<< runMyMonad cnt do
		ids <- takeBytes 2
		print' $ ids == ids0
		cm <- pop
		fs <- maybe (throwError "bad flags") pure . readFlags =<< pop
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
