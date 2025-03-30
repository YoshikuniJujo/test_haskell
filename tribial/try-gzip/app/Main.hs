{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.Pipe
import Data.ByteString qualified as BS
import System.Environment

import Gzip
import MyMonadNew
import MonadByteString qualified as BS
import BitArray
import MonadBitArray qualified as BA

import Control.Monad.Base
import Control.MonadClasses.State qualified as MC
import Control.MonadClasses.Except qualified as MC

import HuffmanTree
import MonadHuffman

main :: IO ()
main = do
	fp : _ <- getArgs
	cnt <- BS.readFile fp
	putStrLn . take 200 . show =<< runMyPipe (fixedTable, fixedTable) (bsToBitArray cnt) tryReadGzip

tryReadGzip :: (
	MC.MonadError String m,
	MonadBase IO m,
	MC.MonadState BS.ByteString m, MC.MonadState BitArray m,
	MC.MonadState (BinTree Int, BinTree Int) m ) => m ()
tryReadGzip = do
		ids <- BS.takeBytes 2
		BS.print' $ ids == ids0
		cm <- BS.pop
		fs <- maybe (MC.throwError @String "bad flags") pure . readFlags =<< BS.pop
		mt <- BS.takeWord32
		efs <- BS.pop
		os <- BS.pop
		fn <- BS.takeString
		BS.print' $ GzipHeader {
			gzipHeaderCompressionMethod = cm,
			gzipHeaderFlags = fs,
			gzipHeaderModificationTime = mt,
			gzipExtraFlags = efs,
			gzipOperatingSystem = os,
			gzipFileName = fn }
		BS.print' =<< BA.pop
		BS.print' =<< BA.takeBit8 2
--		BA.bits =$= huffmanPipe =$= (BS.print' =<< await)
		BS.print' =<< huffStep =<< BA.pop'
		BS.print' =<< huffStep =<< BA.pop'
		BS.print' =<< huffStep =<< BA.pop'
		BS.print' =<< huffStep =<< BA.pop'
		BS.print' =<< huffStep =<< BA.pop'
		BS.print' =<< huffStep =<< BA.pop'
		BS.print' =<< huffStep =<< BA.pop'
		BS.print' =<< huffStep =<< BA.pop'
		BS.print' =<< huffStep =<< BA.pop'

{-
		BS.print' =<< BA.byteBoundary
		BS.print' . BS.bits =<< BS.pop
		BS.print' . BS.bits =<< BS.pop
		-}
