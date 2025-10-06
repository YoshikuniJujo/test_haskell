{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.Buffer qualified as Buffer
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.Zlib qualified as PipeZ
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.IO qualified as IO
import Data.ByteString.FingerTree qualified as BSF
import Data.Color
import Data.Png.Header qualified as Header
import System.IO
import System.Environment
import System.FilePath

import Codec.Compression.Zlib.Constant.Core qualified as Zlib

import Data.Image.Simple qualified as Image

import Control.Monad.Yaftee.Pipe.Png.Encode qualified as Encode

import "try-gzip-yaftee-zlib-ft" Tools

main :: IO ()
main = do
	fpo : _ <- getArgs

	let	hdr' = Header.Header {
			Header.headerWidth = 30, Header.headerHeight = 30,
			Header.headerBitDepth = 8, Header.headerColorType = Header.ColorType 6,
			Header.headerCompressionMethod = Header.CompressionMethodDeflate,
			Header.headerFilterMethod = Header.FilterMethodDefaultFilter,
			Header.headerInterlaceMethod = Header.InterlaceMethodNon }

	img <- Image.new
		(fromIntegral $ Header.headerWidth hdr')
		(fromIntegral $ Header.headerHeight hdr')
	(\y -> (\x -> Image.write @IO img x y $ RgbaWord8 @Double 0 255 0 255) `mapM_` [0 .. 29]) `mapM_` [0 .. 29]

	ibe <- PipeZ.cByteArrayMalloc 64
	obe <- PipeZ.cByteArrayMalloc 64

	let	(fpbd, fpex) = splitExtension fpo
		fpo' = fpbd ++ "-img" <.> fpex
	ho <- openFile fpo' WriteMode

	void . Eff.runM . Except.run @String . Except.run @Zlib.ReturnCode
		. Buffer.run @"barbaz" @BSF.ByteString
		. PipeZ.run @"barbaz"
		. Pipe.run
		. (`Except.catch` IO.putStrLn)
		. void $ fromImage @Double IO img (Header.headerToPoss' hdr')
			Pipe.=$= Encode.encodeRgba "barbaz" IO hdr' ibe obe
			Pipe.=$= PipeT.convert BSF.toStrict
			Pipe.=$= PipeBS.hPutStr ho

	hClose ho

data Chunk = Chunk { chunkName :: BSF.ByteString, chunkBody :: BSF.ByteString }
	deriving Show
