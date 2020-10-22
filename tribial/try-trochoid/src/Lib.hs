{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Control.Arrow
import Control.Monad.ST
import Data.Set
import Codec.Picture

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.ImageSurfaces
import Graphics.Cairo.Paths
import Graphics.Cairo.CairoPatternT
import Graphics.Cairo.Types
import Graphics.Cairo.Values
import Graphics.Cairo.PngSupport

dot :: Double -> Rational -> Double -> (Double, Double)
dot r rt x = (
	200 * cos x - r * sin (fromRational rt * x) + 400,
	200 * sin x - r * cos (fromRational rt * x) + 400 )

dotList :: Double -> Rational -> [(Double, Double)]
dotList r rt = dot r rt <$> [0, pi / 1024 .. 128 * pi]

dot2 :: Double -> (Double, Double)
dot2 x = (
	200 * cos x - 140 * sin (17 / 16 * x) + 400,
	200 * sin x - 140 * cos (17 / 16 * x) + 400 )

dots :: Set (Int, Int)
dots = fromList $ ((round *** round) . dot 120 (18 / 17)) <$> [0, pi / 1024 .. 128 * pi]

dots2 :: Set (Int, Int)
dots2 = fromList $ ((round *** round) . dot2) <$> [0, pi / 1024 .. 128 * pi]

pixel :: Int -> Int -> PixelRGB8
pixel x y
--	| member (x, y) dots2 = PixelRGB8 0xff 0x00 0x00
--	| member (x, y) dots = PixelRGB8 0x00 0xff 0x00
	| member (x, y) dots = PixelRGB8 0x7f 0x7f 0x00
	| otherwise = PixelRGB8 0xff 0xff 0xff

img :: Image PixelRGB8
img = generateImage pixel 800 800

writeTrochoid :: IO ()
writeTrochoid = writePng "tmp.png" img

writeWithCairo1 :: IO ()
writeWithCairo1 = writeWithCairo "tmp_c.png" 120 (18 / 17)

writeWithCairo :: FilePath -> Double -> Rational -> IO ()
writeWithCairo fp r rt = do
	s <- cairoImageSurfaceCreate cairoFormatArgb32 800 800
	cr <- cairoCreate s
--	cairoSetSourceRgb cr 0.5 0.5 0
	cairoSetLineWidth cr 10
	cairoSetSource cr =<< pat1
	polygon cr (head ds) (tail ds)
	cairoStroke cr
	print =<< cairoSurfaceWriteToPng s fp
	where
	ds = dotList r rt

polygon :: CairoT RealWorld -> (Double, Double) -> [(Double, Double)] -> IO ()
polygon cr (x0, y0) ps = do
	cairoMoveTo cr x0 y0
	uncurry (cairoLineTo cr) `mapM_` ps

pat1 :: IO (CairoPatternT RealWorld)
pat1 = do
	p <- cairoPatternCreateRadial 400 400 140 400 400 250
	cairoPatternAddColorStopRgb p 1.0 0.0 0.0 1.0
	cairoPatternAddColorStopRgb p 0.5 1.0 1.0 0
	cairoPatternAddColorStopRgb p 0.0 1.0 0.0 0.2
	pure p

sample1 :: FilePath -> IO ()
sample1 fp = writeWithCairo fp 50 (10 / 8)
