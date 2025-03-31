{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Data.Pipe
import Data.Maybe
import Data.Word
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
	putStrLn . take 200 . show =<< runMyPipe ((fixedTable, fixedTable), ExtraBits 0) (bsToBitArray "") (yield cnt =$= do
		BS.print' =<< readHeader
		BS.print' =<< BA.takeBit8 3
		BA.bits =$= huffmanPipe =$= putDecoded)

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

putDecoded :: (
	PipeClass p,
	MC.MonadState (BinTree Int, BinTree Int) (p (Either Int Word16) o m),
	MC.MonadState ExtraBits (p (Either Int Word16) o m),
	MonadBase IO (p (Either Int Word16) o m),
	Monad m
	) =>
	p (Either Int Word16) o m ()
putDecoded = do
	mi <- await
	BS.print' mi
	case mi of
		Just (Left 256) -> pure ()
		Just (Left i)
			| 0 <= i && i <= 255 -> putDecoded
			| 257 <= i && i <= 264 -> MC.put (fixedDstTable, fixedDstTable) >> putDist
			| 265 <= i && i <= 268 -> do
				MC.put $ ExtraBits 1
				putDecoded
			| otherwise -> error "putDecoded: yet"
		Just (Right eb) -> do
			MC.put (fixedDstTable, fixedDstTable)
			putDist
		Nothing -> pure ()

putDist :: (
	PipeClass p,
	MC.MonadState (BinTree Int, BinTree Int) (p (Either Int Word16) o m),
	MC.MonadState ExtraBits (p (Either Int Word16) o m),
	MonadBase IO (p (Either Int Word16) o m),
	Monad m
	) =>
	p (Either Int Word16) o m ()
putDist = do
	mi <- await
	BS.print' mi
	case mi of
		Just (Left i)
			| 0 <= i && i <= 3 -> MC.put (fixedTable, fixedTable) >> putDecoded
			| 4 <= i && i <= 5 -> do
				MC.put $ ExtraBits 1
				putDist
			| otherwise -> error "putDist: yet"
		Just (Right eb) -> do
			MC.put (fixedTable, fixedTable)
			putDecoded
