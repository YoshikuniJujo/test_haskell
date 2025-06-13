{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.C.String
import Control.Monad.ToolsYj
import System.Environment
import Codec.Compression.Zlib.Gzip.Core qualified as Gzip

main :: IO ()
main = do
	fpi : _ <- getArgs
	cfpi <- newCString fpi
	gzfi <- Gzip.c_gzopen cfpi =<< newCString "rb"
	doWhile_ $ allocaBytes 64 \bf -> do
		r <- Gzip.c_gzgets gzfi bf 64
		if r == nullPtr
			then pure False
			else True <$ (putStr =<< peekCString bf)
	print =<< Gzip.c_gzclose gzfi
