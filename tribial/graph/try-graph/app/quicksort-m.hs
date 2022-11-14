{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.Types
import Control.Arrow
import Control.Monad.Primitive
import Data.Foldable
import Data.Maybe
import Data.Text qualified as T
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
import Text

translate :: (CDouble, CDouble) -> (CDouble, CDouble)
translate (x, y) = (transX x, transY y)

transX, transY :: CDouble -> CDouble
transX x = log x * 150 + 120
transY y = 768 - (y - 0.110) * 5000

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
	cairoMoveTo cr (transX 9) 200
	cairoLineTo cr (transX 9) 700
	cairoStroke cr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.5 0.5 0.5
	cairoMoveTo cr (transX 1) 715
	cairoLineTo cr (transX 256) 715
	cairoStroke cr
	for_ [1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024] \i -> do
		cairoMoveTo cr (transX i) 715
		cairoLineTo cr (transX i) 700
		cairoStroke cr
		putText cr (Size 10) (transX i - 5) 730
			. T.pack . show @Int $ round i
	cairoMoveTo cr 60 (transY 0.15)
	cairoLineTo cr 60 (transY 0.25)
	cairoStroke cr
	for_ [0.15, 0.2, 0.25] \s -> do
		cairoMoveTo cr 60 (transY s)
		cairoLineTo cr 70 (transY s)
		cairoStroke cr
		putText cr (Size 10) 25 (transY s - 9) . T.pack $ show s

	cairoSetLineWidth cr 0.3
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.8 0.3 0.3
	draw1 cr `mapM_` as
--	graph cr $ translate <$> ps
	cairoStroke cr

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
