{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Arrow
import Control.Monad
import Control.Monad.ST
import Foreign.C.Types
import Data.Foldable
import Data.Traversable
import Data.Maybe
import Data.Char
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time
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
	let	hd0 = ["Data.List", "merge", "heap", "quick"]
	als <- for fps \fp -> do
		al <- readAll <$> readFile fp
		let	hd = header al
		print $ machine al
		print hd
		when (hd /= hd0) $ error "no mutch data"
		pure al
	mkGraph hd0 als

mkGraph :: [T.Text] -> [All] -> IO ()
mkGraph hd0 als = withCairo "try-sorts.png" 1024 768 \cr -> do
	cairoSetLineWidth cr 0.5

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.5 0.5 0.5
	cairoMoveTo cr (transX $ 10 ^ (3 :: Int)) (transY 0)
	cairoLineTo cr (transX $ 10 ^ (6 :: Int)) (transY 0)
	cairoStroke cr
	for_ [10 ^ (3 :: Int), 10 ^ (4 :: Int), 10 ^ (5 :: Int), 10 ^ (6 :: Int)] \i -> do
		cairoMoveTo cr (transX i) 668
		cairoLineTo cr (transX i) 653
		cairoStroke cr
		putText cr (Size 10) (transX i - 15) 675 . T.pack $ show i

	cairoMoveTo cr 115 (transY $ 5 * 10 ** (- 8))
	cairoLineTo cr 115 (transY $ 40 * 10 ** (- 8))
	cairoStroke cr
	for_ ([5 * 10 ** (- 8), 10 * 10 ** (- 8) .. 40 * 10 ** (- 8)] `zip` cycle [False, True]) \(i, b) -> do
		cairoMoveTo cr 115 (transY i)
		cairoLineTo cr 130 (transY i)
		cairoStroke cr
		when b . putText cr (Size 10) 70 (transY i - 7) . T.pack $ show i

	putText cr (Size 15) 500 700 "N"
	putTextRot90 cr (Size 15) 30 450 "Second / (N log N)"

	for_ (colors `zip` hd0 `zip` [0 ..]) \((clr, hd), i) -> do
		cairoSetSourceRgb cr clr
		cairoMoveTo cr 700 (100 + 30 * i)
		cairoLineTo cr 760 (100 + 30 * i)
		cairoStroke cr
		putText cr (Size 12) 775 (100 - 10 + 30 * i) hd
	mid <- T.dropWhileEnd isSpace <$> T.readFile "/etc/machine-id"
	print mid
	let	fltrd = filter ((== mid) . machine) als
	print $ length fltrd
	for_ fltrd \al -> let dt = dat al in
		uncurry (drawLines cr) `mapM_` (
			colors `zip`
			((translate . resultToCDouble <$>) <$> dt))

resultToCDouble :: (Integer, NominalDiffTime) -> (CDouble, CDouble)
resultToCDouble (fromIntegral -> n, t) = (n, tr $ realToFrac t)
	where tr = (/ (n * log n))

colors :: [Rgb CDouble]
colors = fromJust . (\(r, g, b) -> rgbDouble r g b) <$> [
	(0.8, 0.3, 0.3), (0.3, 0.8, 0.3), (0.3, 0.3, 0.8), (0.6, 0.6, 0.2) ]

drawLines :: CairoT s RealWorld -> Rgb CDouble -> [(CDouble, CDouble)] -> IO ()
drawLines _ _ [] = pure ()
drawLines cr clr ((x0, y0) : xys) = do
	cairoSetSourceRgb cr clr
	cairoMoveTo cr x0 y0
	for_ xys \(x, y) -> cairoLineTo cr x y
	cairoStroke cr

translate :: (CDouble, CDouble) -> (CDouble, CDouble)
translate = transX *** transY

transX, transY :: CDouble -> CDouble
transX x = log (x / 1000) * 115 + 150
transY y = (- y * 1.5 * 10 ^ (9 :: Int)) + 668
