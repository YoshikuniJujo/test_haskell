{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.Types
import Control.Monad.ST
import Data.Foldable
import Data.Traversable
import Data.Maybe
import Data.Text qualified as T
import Data.Time
import Data.Hason
import Data.Color
import Data.CairoContext
import System.Environment
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths

import Cairo
import Text
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
		cairoMoveTo cr (transX 1000) (transY 0)
		cairoLineTo cr (transX 8000) (transY 0)
		cairoStroke cr
		for_ [1000, 2000 .. 8000] \x -> do
			cairoMoveTo cr (transX x) (transY 0)
			cairoLineTo cr (transX x) (transY 0 - 10)
			cairoStroke cr
		for_ [2000, 4000 .. 8000] \x -> do
			putText cr (Size 10) (transX x - 15) (transY 0 + 10) . T.pack $ show x

translate :: (Integer, NominalDiffTime) -> (CDouble, CDouble)
translate (n, t) = (transX n, transY $ realToFrac t / fromIntegral n ^ (2 :: Int))

transX :: Integer -> CDouble
transX n = fromIntegral n / 10

transY :: CDouble -> CDouble
transY y = (768 - 100) - y * 500 * 25 * 10 ^ (6 :: Int)

drawLines :: CairoT s RealWorld -> Rgb CDouble -> [(CDouble, CDouble)] -> IO ()
drawLines _ _ [] = pure ()
drawLines cr clr ((x0, y0) : xys) = do
	cairoSetSourceRgb cr clr
	cairoSetLineWidth cr 0.5
	cairoMoveTo cr x0 y0
	for_ xys \(x, y) -> cairoLineTo cr x y
	cairoStroke cr
