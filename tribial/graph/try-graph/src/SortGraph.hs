{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIOnS_GHC -Wall -fno-warn-tabs #-}

module SortGraph where

import Foreign.C.Types
import Control.Arrow
import Control.Monad.ST
import Data.Foldable
import Data.Maybe
import Data.Char
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time
import Data.Color
import Data.CairoContext
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths

import Cairo
import Text
import SortResultHason

mkGraph :: FilePath -> CDouble -> CDouble -> [T.Text] -> [All] -> IO ()
mkGraph fp minY maxY hd0 als = withCairo fp 1024 768 \cr -> do
	cairoSetLineWidth cr 0.5

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.5 0.5 0.5
	cairoMoveTo cr (transX $ 10 ^ (3 :: Int)) 668
	cairoLineTo cr (transX $ 10 ^ (6 :: Int)) 668
	cairoStroke cr
	for_ [10 ^ (3 :: Int), 10 ^ (4 :: Int), 10 ^ (5 :: Int), 10 ^ (6 :: Int)] \i -> do
		cairoMoveTo cr (transX i) 668
		cairoLineTo cr (transX i) 653
		cairoStroke cr
		putText cr (Size 10) (transX i - 15) 675 . T.pack $ show i

	cairoMoveTo cr 115 (transY minY maxY minY)
	cairoLineTo cr 115 (transY minY maxY maxY)
	cairoStroke cr
	for_ (tick (5 * 10 ** (- 8)) minY maxY) \i -> do
		cairoMoveTo cr 115 (transY minY maxY i)
		cairoLineTo cr 130 (transY minY maxY i)
		cairoStroke cr
	for_ (tick (5 * 10 ** (- 8)) minY maxY) \i ->
		putText cr (Size 10) 70 (transY minY maxY i - 7) . T.pack $ show i

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
			((translate minY maxY . resultToCDouble <$>) <$> dt))

tick :: CDouble -> CDouble -> CDouble -> [CDouble]
tick itv mn mx = [t0, t0 + itv .. mx]
	where
	t0 = itv * (fromIntegral $ ceiling @_ @Int (mn / itv))

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

translate :: CDouble -> CDouble -> (CDouble, CDouble) -> (CDouble, CDouble)
translate mny mxy = transX *** transY mny mxy

transX :: CDouble -> CDouble
transX x = log (x / 1000) * 115 + 150

transY :: CDouble -> CDouble -> CDouble -> CDouble
transY mn mx y = (- (y - mn) / (mx - mn) * 3.5 * 1.5 * 10 ^ (2 :: Int)) + 593
