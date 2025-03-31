{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.Pipe
import Data.Maybe
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

import PipeByteString qualified as PBS

main :: IO ()
main = do
	fp : _ <- getArgs
	cnt <- BS.readFile fp
	putStrLn . take 200 . show =<< runMyPipe (fixedTable, fixedTable) (bsToBitArray "") (yield cnt =$= do
		BS.print' =<< readHeader
		BS.print' =<< BA.takeBit8 3
		BA.bits =$= huffmanPipe =$= ((BS.print' =<< await) >> (BS.print' =<< await)))

readHeader :: (
	PipeClass p,
	MC.MonadState BS.ByteString (p BS.ByteString o m),
	MC.MonadError String (p BS.ByteString o m),
	MonadFail (p BS.ByteString o m),
	MonadBase IO (p BS.ByteString o m),
	Monad m ) =>
	p BS.ByteString o m GzipHeader
readHeader = do
		Just ids <- PBS.takeBytes 2
		BS.print' $ ids == ids0
		Just cm <- PBS.popByte
		fs <- maybe (MC.throwError @String "bad flags") pure . readFlags . fromJust =<< PBS.popByte
		Just mt <- PBS.takeWord32
		Just efs <- PBS.popByte
		Just os <- PBS.popByte
		Just fn <- PBS.takeString
		pure GzipHeader {
			gzipHeaderCompressionMethod = cm,
			gzipHeaderFlags = fs,
			gzipHeaderModificationTime = mt,
			gzipExtraFlags = efs,
			gzipOperatingSystem = os,
			gzipFileName = fn }
