{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.Ptr
import Foreign.C.Types

import Data.Maybe
import Data.CairoImage
import Data.JuicyCairo
import Data.Color
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

import Lib

main :: IO ()
main = do
	sr <- cairoImageSurfaceCreate cairoFormatArgb32 128 128
	cr <- cairoCreate sr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.2 0.6 0.1
	cairoRectangle cr 32 32 64 64
	pth <- cairoCopyPath cr
	print pth
	withCairoPathT pth \ppth -> do
		pd <- cairoPathTData ppth
		nd <- cairoPathTNumData ppth
		tryCairoPathData pd nd
		{-
		print pd
		print nd
		print =<< cairoPathDataTHeaderType pd
		ln <- cairoPathDataTHeaderLength pd
		print ln
		let	pd1 = nextCairoPathDataT pd
		print =<< cairoPathDataTPointX pd1
		print =<< cairoPathDataTPointY pd1
		let	pd2 = nextByLength pd ln
		print =<< cairoPathDataTHeaderType pd2
		print =<< cairoPathDataTHeaderLength pd2
		let	pd3 = nextCairoPathDataT pd2
		print =<< cairoPathDataTPointX pd3
		print =<< cairoPathDataTPointY pd3
		-}
	cairoFill cr
	cairoImageSurfaceGetCairoImage sr >>= \case
		CairoImageArgb32 ci ->
			writePng "try-cairo-path-t-exe.png" $ cairoArgb32ToJuicyRGBA8 ci
		_ -> error "never occur"

tryCairoPathData :: Ptr CairoPathDataT -> CInt -> IO ()
tryCairoPathData _ n | n < 1 = pure ()
tryCairoPathData p n = do
	cairoPathDataTHeaderType p >>= \case
		0 -> printMove p
		1 -> printLine p
		2 -> print 2
		3 -> putStrLn "Close"
	ln <- cairoPathDataTHeaderLength p
	tryCairoPathData (nextByLength p ln) (n - ln)

printMove :: Ptr CairoPathDataT -> IO ()
printMove p = do
	putStr "MoveTo "
	putStr . show =<< cairoPathDataTPointX (nextCairoPathDataT p)
	putStr " "
	print =<< cairoPathDataTPointY (nextCairoPathDataT p)

printLine :: Ptr CairoPathDataT -> IO ()
printLine p = do
	putStr "LineTo "
	putStr . show =<< cairoPathDataTPointX (nextCairoPathDataT p)
	putStr " "
	print =<< cairoPathDataTPointY (nextCairoPathDataT p)
