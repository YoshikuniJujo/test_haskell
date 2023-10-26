{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.CairoImage
import Graphics.Cairo.Values
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces

import Data.Maybe
import Data.Color
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Drawing.Paths

import Control.Monad.Primitive
import Control.Monad.ST

import SampleImages

main :: IO ()
main = do
	putStrLn "*** TEST ARGB 32 BEGIN ***"

	case runST twoRectangles of
		CairoImageArgb32 i -> writeArgb32 "helloWriteImageSurface.png" i
		_ -> error "image format error"

	putStrLn "*** TEST ARGB 32 END ***"

writeArgb32 :: FilePath -> Argb32 -> IO ()
writeArgb32 fp = writePng fp . cairoArgb32ToJuicyRGBA8
