{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.Types
import Control.Arrow
import Control.Monad.Primitive
import Data.Foldable
import Data.Maybe
import Data.CairoContext
import Data.CairoImage.Internal
import Data.JuicyCairo
import Data.Color
import Codec.Picture
import System.Environment
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values hiding (CairoFormatT, cairoFormatArgb32)

import Data.Time

translate :: (CDouble, CDouble) -> (CDouble, CDouble)
translate (x, y) = (transX x, transY y)

transX, transY :: CDouble -> CDouble
transX x = log x * 160 + 50
transY y = 768 - (y - 1.0) * 360

draw1 :: CairoT s RealWorld -> FilePath -> IO ()
draw1 cr fp = do
	ps <- ((\[m, t] -> (read m :: CDouble, read (init t) :: CDouble)) . words <$>)
			. lines <$> readFile fp
	graph cr $ translate <$> ps

main :: IO ()
main = do
	as <- getArgs
	sr <- cairoImageSurfaceCreate CairoFormatArgb32 1024 768
	cr <- cairoCreate sr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 1.0 1.0 1.0
	cairoPaint cr
	cairoSetLineWidth cr 0.5
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.8 0.8 0.8
	cairoMoveTo cr (transX 9) (transY 1.1)
	cairoLineTo cr (transX 9) (transY 2.5)
	cairoStroke cr
	cairoSetLineWidth cr 0.3
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.8 0.3 0.3
	draw1 cr `mapM_` as
--	graph cr $ translate <$> ps
	cairoStroke cr
	{-
	[a1, a1_5, a2, a2_3, a2_5, a3, a4] <- map read . words <$> readFile "amounts.txt"

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.9 0.9 0.9
	cairoMoveTo cr (dayToX $ fromGregorian 2017 12 24) (amountToY a1)
	cairoLineTo cr (dayToX $ fromGregorian 2021  4 30) (amountToY a1)
	cairoMoveTo cr (dayToX $ fromGregorian 2017 12 24) (amountToY a2)
	cairoLineTo cr (dayToX $ fromGregorian 2021  4 30) (amountToY a2)
	cairoMoveTo cr (dayToX $ fromGregorian 2017 12 24) (amountToY a2_5)
	cairoLineTo cr (dayToX $ fromGregorian 2021  4 30) (amountToY a2_5)
	cairoStroke cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.7 0.7 0.7
	cairoMoveTo cr (dayToX $ fromGregorian 2017 12 24) (amountToY $ a1_5 - 10000000)
	cairoLineTo cr (dayToX $ fromGregorian 2021  4 30) (amountToY $ a1_5 - 10000000)
	cairoMoveTo cr (dayToX $ fromGregorian 2017 12 24) (amountToY a1_5)
	cairoLineTo cr (dayToX $ fromGregorian 2021  4 30) (amountToY a1_5)
	cairoMoveTo cr (dayToX $ fromGregorian 2017 12 24) (amountToY a2_3)
	cairoLineTo cr (dayToX $ fromGregorian 2021  4 30) (amountToY a2_3)
	cairoMoveTo cr (dayToX $ fromGregorian 2018  1  1) (amountToY a3)
	cairoLineTo cr (dayToX $ fromGregorian 2018  1  1) (amountToY a4)
	cairoMoveTo cr (dayToX $ fromGregorian 2019  1  1) (amountToY a3)
	cairoLineTo cr (dayToX $ fromGregorian 2019  1  1) (amountToY a4)
	cairoMoveTo cr (dayToX $ fromGregorian 2020  1  1) (amountToY a3)
	cairoLineTo cr (dayToX $ fromGregorian 2020  1  1) (amountToY a4)
	cairoMoveTo cr (dayToX $ fromGregorian 2021  1  1) (amountToY a3)
	cairoLineTo cr (dayToX $ fromGregorian 2021  1  1) (amountToY a4)
	cairoStroke cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.2 0.6 0.1
	dat <- readData "data.txt"
	let	ps = (dayToX *** amountToY) <$> dat
	print ps
	graph cr ps
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.6 0.4 1.0
	dat2 <- readData "data2.txt"
	let	ps2 = (dayToX *** amountToY . (+ 20000000)) <$> dat2
	graph cr ps2

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.2 0.5 0
	crr <- read <$> readFile "current.txt"
	cairoMoveTo cr (dayToX $ fromGregorian 2021  3 25) (amountToY crr)
	cairoLineTo cr (dayToX $ fromGregorian 2021  4  5) (amountToY crr)
	cairoStroke cr
	-}

	cairoImageSurfaceGetCairoImage sr >>= \case
		CairoImageArgb32 ci ->
			writePng "quicksort-m.png" $ cairoArgb32ToJuicyRGBA8 ci
		_ -> error "never occur"

readData :: FilePath -> IO [(Day, Int)]
readData fp = do
	wss <- map words . lines <$> readFile fp
	pure $ map (\[d, a] -> (read d, read a)) wss

dayToX :: Day -> CDouble
dayToX d = fromIntegral (d `diffDays` fromGregorian 2017 12 24) / 2 + 32

amountToY :: Int -> CDouble
amountToY a = 512 - fromIntegral (a - 20000000) / 50000 - 16

graph :: PrimMonad m => CairoT s (PrimState m) -> [(CDouble, CDouble)] -> m ()
graph _ [] = pure ()
graph cr ((x0, y0) : xys) = do
	cairoMoveTo cr x0 y0
	for_ xys \(x, y) -> cairoLineTo cr x y
	cairoStroke cr
