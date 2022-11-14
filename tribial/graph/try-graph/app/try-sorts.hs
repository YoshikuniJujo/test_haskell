{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Arrow
import Control.Monad
import Control.Monad.ST
import Foreign.C.Types
import Data.Foldable
import Data.Maybe
import Data.List qualified as L
import Data.Text qualified as T
import Data.Color
import Data.CairoContext
import System.Environment
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths

import Cairo
import Text

main :: IO ()
main = do
	fps <- getArgs
	withCairo "try-sorts.png" 1024 768 \cr -> do
		cairoSetLineWidth cr 0.5

		cairoSetSourceRgb cr . fromJust $ rgbDouble 0.5 0.5 0.5
		cairoMoveTo cr (transX $ 10 ^ 3) (transY 0)
		cairoLineTo cr (transX $ 10 ^ 6) (transY 0)
		cairoStroke cr
		for_ [10 ^ 3, 10 ^ 4, 10 ^ 5, 10 ^ 6] \i -> do
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

		let	hd0 = ["Data.List", "merge", "heap", "quick"]

		for_ (colors `zip` hd0 `zip` [0 ..]) \((clr, hd), i) -> do
			cairoSetSourceRgb cr clr
			cairoMoveTo cr 700 (100 + 30 * i)
			cairoLineTo cr 760 (100 + 30 * i)
			cairoStroke cr
			putText cr (Size 12) 775 (100 - 10 + 30 * i) $ T.pack hd

		for_ fps \fp -> do
			cnt <- readFile fp
			let	hd = readHeader cnt
				dat = readData cnt
			print hd
			when (hd /= hd0) $ error "no mutch data"
			uncurry (drawLines cr) `mapM_` (colors `zip` ((translate <$>) <$> dat))
--	drawLines cr [(100, 100), (200, 200)]

colors :: [Rgb CDouble]
colors = fromJust . (\(r, g, b) -> rgbDouble r g b) <$> [
	(0.8, 0.3, 0.3), (0.3, 0.8, 0.3), (0.3, 0.3, 0.8), (0.6, 0.6, 0.2) ]

readHeader :: String -> [String]
readHeader = spans (/= '/') . (!! 1) . words . head . lines

readData :: String -> [[(CDouble, CDouble)]]
readData = L.transpose . (separate . readData1 <$>) . tail . lines

separate :: (a, [a]) -> [(a, a)]
separate (x, ys) = (x ,) <$> ys

readData1 :: String -> (CDouble, [CDouble])
readData1 str = case span (/= '\t') str of
	(n_, '\t' : dat) -> let
		n = read n_
		tr = (/ (n * log n)) . read . takeWhile (/= 's') in case spans (/= '/') dat of
		ds -> (n, tr <$> ds)
		_ -> error "bad format"
	_ -> error "bad format"

spans :: (a -> Bool) -> [a] -> [[a]]
spans p = \case
	[] -> [[]]
	x : xs	| p x -> (x : y) : ys
		| otherwise -> [] : ya
		where ya@(y : ys) = spans p xs

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
transY y = (- y * 1.5 * 10 ^ 9) + 668
