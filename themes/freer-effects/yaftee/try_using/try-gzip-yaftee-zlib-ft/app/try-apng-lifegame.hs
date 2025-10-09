{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import GHC.Stack

import Data.Ratio
import Data.Word
import Data.ByteString.FingerTree qualified as BSF
import Data.Image.Immutable qualified as ImageI
import Data.Png.Header qualified as Header

import System.Environment
import System.FilePath
import System.File.Apng

import Lifegame

main :: IO ()
main = do
	fpo : _ <- getArgs
	writeApngGray' (filePathGray fpo) header images'

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

images :: HasCallStack => [(ImageI.Gray, Ratio Word16)]
images = take 50
	$ (, 0.5) . boardToGray 10 <$> boards (listToBoard $ putShapeAscii 10 10 3 3 glider)

images' :: HasCallStack => [(ImageI.Gray, Ratio Word16)]
images' = take 15 . drop 2
	$ (, 0.5) . boardToGray 10 <$> boards (listToBoard $ putShapeAscii 20 11 5 5 penta)
