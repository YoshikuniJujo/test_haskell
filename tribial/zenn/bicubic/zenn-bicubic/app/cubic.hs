{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.Types
import Data.Foldable
import Data.Maybe
import Data.Color
import Data.CairoImage.Internal
import Data.JuicyCairo
import Codec.Picture
import System.Random

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoT.Setting
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.ImageSurfaces

import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Rendering.Cairo

import qualified Data.Text as T

import Data.Array

leftMargin, rightMargin, topMargin, bottomMargin :: CDouble
leftMargin = 20; rightMargin = 20; topMargin = 20; bottomMargin = 20

unit :: CDouble
unit = 30

number :: CDouble
number = 12

radius :: CDouble
radius = 5

radius' :: CDouble
radius' = 3

ceiling' = (+ 1) . floor

main :: IO ()
main = do
	s <- cairoImageSurfaceCreate CairoFormatArgb32
		(floor $ leftMargin + unit * number + rightMargin)
		(floor $ topMargin + unit * number + bottomMargin)
	cr <- cairoCreate s

	for_ [1, 1 + 1 / 3 .. 3] \y ->
		for_ [1, 1 + 1 / 3 .. 3] \x -> do
			let	(xi, xd) = properFraction x
				(yi, yd) = properFraction y
				cs = (<$> [yi - 1 .. yi + 2]) \a ->
					(<$> [xi - 1 .. xi + 2]) \b -> colorsA ! a ! b
				-- c00 = colorsA !  yi ! xi
				c00 = cs !! 1 !! 1
				c10 = cs !! 1 !! 2
				c01 = cs !! 2 !! 1
				c11 = cs !! 2 !! 2
				c0 = (c00 `mul` (fromIntegral (ceiling' x) - x)) `add` (c10 `mul` (x - fromIntegral (floor x)))
				c1 = (c01 `mul` (fromIntegral (ceiling' x) - x)) `add` (c11 `mul` (x - fromIntegral (floor x)))
				c = (c0 `mul` (fromIntegral (ceiling' y) - y)) `add` (c1 `mul` (y - fromIntegral (floor y)))
			cairoSetSourceRgb cr c
			cairoRectangle cr
				(leftMargin + 3 * unit * (x - 1 / 6))
				(topMargin + 3 * unit * (y - 1 / 6))
				(3 * unit * (1 / 3))
				(3 * unit * (1 / 3))
			cairoFill cr

	cairoSet cr $ LineWidth 1
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.6 0.6 0.6

	for_ [0 .. number] \y -> do
		cairoMoveTo cr 0 (topMargin + unit * y)
		cairoLineTo cr (leftMargin + unit * number + rightMargin) (topMargin + unit * y)

	for_ [0 .. number] \x -> do
		cairoMoveTo cr (leftMargin + unit * x) 0
		cairoLineTo cr (leftMargin + unit * x) (topMargin + unit * number + bottomMargin)

	cairoStroke cr

	for_ [1, 1 + 1 / 3 .. 3] \y ->
		for_ [1, 1 + 1 / 3 .. 3] \x -> do
			let	c00 = colorsA ! floor y ! floor x
				c10 = colorsA ! floor y ! ceiling' x
				c01 = colorsA ! ceiling' y ! floor x
				c11 = colorsA ! ceiling' y ! ceiling' x
				c0 = (c00 `mul` (fromIntegral (ceiling' x) - x)) `add` (c10 `mul` (x - fromIntegral (floor x)))
				c1 = (c01 `mul` (fromIntegral (ceiling' x) - x)) `add` (c11 `mul` (x - fromIntegral (floor x)))
				c = (c0 `mul` (fromIntegral (ceiling' y) - y)) `add` (c1 `mul` (y - fromIntegral (floor y)))
			cairoMoveTo cr
				(leftMargin + 3 * unit * x + radius')
				(topMargin + 3 * unit * y)
			cairoArc cr
				(leftMargin + 3 * unit * x)
				(topMargin + 3 * unit * y)
				radius' 0 (2 * pi)
			cairoSet cr $ LineWidth 2
			cairoSetSourceRgb cr . fromJust $ rgbDouble 0.5 0.5 0.5
			cairoStrokePreserve cr
			cairoSetSourceRgb cr c
			cairoFill cr

	for_ (zip [0 .. 4] colors) \(y, cs) ->
		for_ (zip [0 .. 4] cs) \(x, c) -> do
			cairoMoveTo cr
				(leftMargin + 3 * unit * x + radius)
				(topMargin + 3 * unit * y)
			cairoArc cr
				(leftMargin + 3 * unit * x)
				(topMargin + 3 * unit * y)
				radius 0 (2 * pi)
			cairoSet cr $ LineWidth 2
			cairoSetSourceRgb cr . fromJust $ rgbDouble 0.5 0.5 0.5
			cairoStrokePreserve cr
			cairoSetSourceRgb cr c
			cairoFill cr
			

{-
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0 0 0
	pl <- pangoCairoCreateLayout cr
	fd <- pangoFontDescriptionNew
	fd' <- pangoFontDescriptionFreeze fd
	pangoLayoutSet pl . pangoFontDescriptionToNullable $ Just fd'
	pangoLayoutSet @T.Text pl "あいうえお\nfoobar"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl
-}

	cairoImageSurfaceGetCairoImage s >>= \case
		CairoImageArgb32 a -> writePng "cubic.png" $ cairoArgb32ToJuicyRGBA8 a
		_ -> error "never occur"

colors :: [[Rgb CDouble]]
colors = sep 5 randomColors
{-
colors = (fromJust . uncurry3 rgbDouble . div100 <$>) <$> [
	[(0, 0, 0), (0, 0, 0), (0, 0, 0), (0, 0, 0), (0, 0, 0)],
	[(0, 0, 0), (0, 0, 0), (0, 0, 0), (0, 0, 0), (0, 0, 0)],
	[(0, 0, 0), (0, 0, 0), (0, 0, 0), (0, 0, 0), (0, 0, 0)],
	[(0, 0, 0), (0, 0, 0), (0, 0, 0), (0, 0, 0), (0, 0, 0)],
	[(0, 0, 0), (0, 0, 0), (0, 0, 0), (0, 0, 0), (0, 0, 0)] ]
	-}

colorsA :: Array Int (Array Int (Rgb CDouble))
colorsA = listArray (0, 4) (listArray (0, 4) <$> colors)

sep :: Int -> [a] -> [[a]]
sep n [] = []
sep n xs = take n xs : sep n (drop n xs)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

div100 :: Fractional n => (n, n, n) -> (n, n, n)
div100 (a, b, c) = (a / 100, b / 100, c / 100)

randomColors :: [Rgb CDouble]
randomColors = fromJust . uncurry3 rgbDouble . div100 <$> tuples3 (randomRs (0, 100) (mkStdGen 8))

tuples3 :: [a] -> [(a, a, a)]
tuples3 [] = []
tuples3 [_] = []
tuples3 [_, _] = []
tuples3 (x : y : z : xs) = (x, y, z) : tuples3 xs

newtype Point16 a = Point16 [[a]] deriving Show

{-
mkPoint16s :: [([a], [a])] -> [[Point16 a]]
mkPoint16s (
	(xs, xs'), (ys, ys'), (zs, zs'), (ws, ws') = []
	-}

mul :: Rgb CDouble -> CDouble -> Rgb CDouble
mul (RgbDouble r g b) n = fromJust $ rgbDouble (r * n) (g * n) (b * n)

add :: Rgb CDouble -> Rgb CDouble -> Rgb CDouble
add (RgbDouble r1 g1 b1)  (RgbDouble r2 g2 b2) = fromJust $ rgbDouble (r1 +! r2) (g1 +! g2) (b1 +! b2)

(+!) :: CDouble -> CDouble -> CDouble
a +! b	| a + b < 0 = 0
	| a + b < 1 = a + b
	| otherwise = a + b
