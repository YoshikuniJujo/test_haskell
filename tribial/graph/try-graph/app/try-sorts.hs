{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Arrow
import Control.Monad.ST
import Foreign.C.Types
import Data.Foldable
import Data.Maybe
import Data.List qualified as L
import Data.Color
import Data.CairoContext
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths

import Cairo

import Debug.Trace

main :: IO ()
main = withCairo "try-sorts.png" 1024 768 \cr -> do
	cnt <- readData <$> readFile "../../mastodon/quicksort/graph/cmp_sorts.txt"
	print cnt
	cairoSetLineWidth cr 0.5

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.5 0.5 0.5
	cairoMoveTo cr (transX $ 10 ^ 3) (transY 0)
	cairoLineTo cr (transX $ 10 ^ 6) (transY 0)
	cairoStroke cr

	uncurry (drawLines cr) `mapM_` (colors `zip` ((translate <$>) <$> cnt))
--	drawLines cr [(100, 100), (200, 200)]

colors :: [Rgb CDouble]
colors = fromJust . (\(r, g, b) -> rgbDouble r g b) <$> [
	(0.8, 0.3, 0.3), (0.3, 0.8, 0.3), (0.3, 0.3, 0.8), (0.6, 0.6, 0.2) ]

readData :: String -> [[(CDouble, CDouble)]]
readData = L.transpose . (separate . readData1 <$>) . tail . lines

separate :: (a, [a]) -> [(a, a)]
separate (x, ys) = (x ,) <$> ys

readData1 :: String -> (CDouble, [CDouble])
readData1 str = case span (/= '\t') str of
	(n_, '\t' : dat) -> let
		n = read n_
		tr = (/ (n * log n)) . read . takeWhile (/= 's') in case spans (/= '/') dat of
		ds -> trace (show ds) (n, tr <$> ds)
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
transX x = log (x / 1000) * 120 + 145
transY y = (- y * 1.5 * 10 ^ 9) + 668
