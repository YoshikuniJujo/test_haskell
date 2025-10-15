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
import Data.Bool
import Data.Word
import Data.Image.Gray qualified as ImageI
import Data.Png.Header qualified as Header

import System.Environment
import System.FilePath
import System.File.Apng.Gray1.NoInterlace

import LifegameBools

main :: IO ()
main = do
	fpi : fpo : _ <- getArgs
	src <- lines <$> readFile fpi
	let	(rt, w, h, xo, yo, drp, fnm, dly, shp) = readLifegame src
		(rpt, img) = mkImages rt w h xo yo drp fnm dly shp
	writeApngGray1' (filePathGray fpo) header fnm (bool 1 0 rpt) img

filePathGray :: FilePath -> FilePath
filePathGray fpo = fpbd ++ "-gray1" <.> fpex
	where (fpbd, fpex) = splitExtension fpo

header :: Header.Header
header = Header.Header {
	Header.headerWidth = 100, Header.headerHeight = 100,
	Header.headerBitDepth = 1,
	Header.headerColorType = Header.ColorTypeGrayscale,
	Header.headerCompressionMethod = Header.CompressionMethodDeflate,
	Header.headerFilterMethod = Header.FilterMethodDefaultFilter,
	Header.headerInterlaceMethod = Header.InterlaceMethodNon }

mkImages ::
	Int -> Int -> Int -> Int -> Int -> Int -> Int -> Ratio Word16 -> [String] ->
	(Bool, [(ImageI.G, Ratio Word16)])
mkImages rt w h xo yo drp nm dly shp =
	((bd0 == bd0'), (, dly) . boardToGray rt <$> bds')
	where
	(bds', bd0' : _) = splitAt nm bds
	bds@(bd0 : _) = drop drp . boards . listToBoard $ putShapeAscii w h xo yo shp
