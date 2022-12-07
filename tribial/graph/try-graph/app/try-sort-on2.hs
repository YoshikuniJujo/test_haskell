{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.Types
import Control.Monad.ST
import Data.Foldable
import Data.Traversable
import Data.Maybe
import Data.Int
import Data.Text qualified as T
import Data.Time
import Data.Hason
import Data.Color
import Data.CairoContext
import System.Environment
import Numeric
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths

import Cairo
import Text
import SortResultHason

width, height :: Int32
width = 1024
height = 768

main :: IO ()
main = do
	fps <- getArgs
	als <- for fps \fp -> do
		al <- readAll <$> readFile fp
		pure $ dat al
	withCairo "try-sort-on2.png" width height \cr -> do
		for_ als \al -> for_ (zip [0 ..] colors) \(i, clr) ->
			drawLines cr clr $ translate <$> (al !! i)
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
		print $ transY 0
		print $ transY (- 40)
		print $ transY (- 50)
		cairoMoveTo cr (transX 0) (transY (5 * 10 ** (- 9)))
		cairoLineTo cr (transX 0) (transY (35 * 10 ** (- 9)))
		cairoStroke cr
		for_ [5 * 10 ** (- 9), 10 * 10 ** (- 9) .. 35 * 10 ** (- 9)] \y -> do
			cairoMoveTo cr (transX 0) (transY y)
			cairoLineTo cr (transX 0 + 10) (transY y)
			cairoStroke cr
		for_ [10 * 10 ** (- 9), 20 * 10 ** (- 9) .. 30 * 10 ** (- 9)] \y -> do
			putText cr (Size 10) (transX 0 - 45) (transY y - 8) . T.pack $ showEFloat (Just 1) y ""
		putTextRot90 cr (Size 15) 30 450 "Second / (N ^ 2)"
		putText cr (Size 15) 575 700 "N"
		for_ (colors `zip` hd0 `zip` [0 ..]) \((clr, hd), i) -> do
			cairoSetSourceRgb cr clr
			cairoMoveTo cr 700 (100 + 30 * i)
			cairoLineTo cr 760 (100 + 30 * i)
			cairoStroke cr
			putText cr (Size 12) 775 (100 - 10 + 30 * i) hd

hd0 = ["insertion", "bubble", "selection"]

translate :: (Integer, NominalDiffTime) -> (CDouble, CDouble)
translate (n, t) = (transX n, transY $ realToFrac t / fromIntegral n ^ (2 :: Int))

transX :: Integer -> CDouble
transX n = 125 + fromIntegral n / 10240 * fromIntegral width

transY :: CDouble -> CDouble
transY y =
	(fromIntegral height - 125) - y * 500 * 28 * 10 ^ (6 :: Int) / 768 * fromIntegral height

drawLines :: CairoT s RealWorld -> Rgb CDouble -> [(CDouble, CDouble)] -> IO ()
drawLines _ _ [] = pure ()
drawLines cr clr ((x0, y0) : xys) = do
	cairoSetSourceRgb cr clr
	cairoSetLineWidth cr 0.5
	cairoMoveTo cr x0 y0
	for_ xys \(x, y) -> cairoLineTo cr x y
	cairoStroke cr

colors :: [Rgb CDouble]
colors = fromJust <$> [
	rgbDouble 0.8 0.3 0.3, rgbDouble 0.3 0.8 0.3, rgbDouble 0.3 0.3 0.8 ]
