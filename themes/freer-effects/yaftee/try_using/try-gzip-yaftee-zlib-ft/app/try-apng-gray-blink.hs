{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.Ratio
import Data.Word
import Data.ByteString.FingerTree qualified as BSF
import Data.Image.Gray qualified as ImageI
import Data.Png.Header qualified as Header
import Data.Png.Header.Data qualified as Header

import System.Environment
import System.FilePath
import System.File.Apng

main :: IO ()
main = do
	fpo : _ <- getArgs
	writeApngGray' (filePathGray fpo) header (length images) 0 images

filePathGray :: FilePath -> FilePath
filePathGray fpo = fpbd ++ "-gray" <.> fpex
	where (fpbd, fpex) = splitExtension fpo

data Chunk = Chunk {
	chunkName :: BSF.ByteString,
	chunkBody :: BSF.ByteString }
	deriving Show

header :: Header.Header
header = Header.Header {
	Header.headerWidth = 100, Header.headerHeight = 100,
	Header.headerBitDepth = 8,
	Header.headerColorType = Header.ColorTypeGrayscale,
	Header.headerCompressionMethod = Header.CompressionMethodDeflate,
	Header.headerFilterMethod = Header.FilterMethodDefaultFilter,
	Header.headerInterlaceMethod = Header.InterlaceMethodNon }

images :: [(ImageI.G, Ratio Word16)]
images = (, 0.1) . ImageI.filled 100 100
	<$> [0x00, 0x03 .. 0xff] ++ [0xfc, 0xf9 .. 0x03]
