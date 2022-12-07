{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.Types
import Control.Monad.ST
import Data.Foldable
import Data.Traversable
import Data.Maybe
import Data.Time
import Data.Hason
import Data.Color
import Data.CairoContext
import System.Environment
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths

import Cairo
import SortResultHason

main :: IO ()
main = do
	fps <- getArgs
	als <- for fps \fp -> do
		al <- readAll <$> readFile fp
		pure $ dat al
	withCairo "try-sort-on2.png" 1024 768 \cr -> do
		for_ als \al -> do
			drawLines cr (fromJust $ rgbDouble 0.8 0.3 0.3) $ translate <$> (al !! 0)
			drawLines cr (fromJust $ rgbDouble 0.3 0.8 0.3) $ translate <$> (al !! 1)
			drawLines cr (fromJust $ rgbDouble 0.3 0.3 0.8) $ translate <$> (al !! 2)
		cairoSetSourceRgb cr (fromJust $ rgbDouble 0.3 0.3 0.3)
		cairoMoveTo cr 100 (transY 0)
		cairoLineTo cr 800 (transY 0)
		cairoStroke cr

translate :: (Integer, NominalDiffTime) -> (CDouble, CDouble)
translate (n, t) = (fromIntegral n / 10, transY $ realToFrac t / fromIntegral n ^ (2 :: Int))

transY :: CDouble -> CDouble
transY y = (768 - 100) - y * 500 * 15 * 10 ^ (6 :: Int)

drawLines :: CairoT s RealWorld -> Rgb CDouble -> [(CDouble, CDouble)] -> IO ()
drawLines _ _ [] = pure ()
drawLines cr clr ((x0, y0) : xys) = do
	cairoSetSourceRgb cr clr
	cairoSetLineWidth cr 0.5
	cairoMoveTo cr x0 y0
	for_ xys \(x, y) -> cairoLineTo cr x y
	cairoStroke cr
