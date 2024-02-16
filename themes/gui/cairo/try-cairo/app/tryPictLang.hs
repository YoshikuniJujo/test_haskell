{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Foreign.C.Types
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Maybe
import Data.Color
import Data.CairoImage.Internal
import Data.CairoContext
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoT.SaveAndRestore
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Drawing.Transformations
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Utilities.CairoMatrixT

result :: Picture
result = pictureLeft 7

pictureLeft :: Int -> Picture
pictureLeft n | n < 1 = empty
pictureLeft n = let
	br = color Brown
		. (!! 3) . iterate half . (!! 6) $ iterate rot45 triangle
	wm = color White
		. (!! 4) . iterate half . (!! 5) $ iterate rot45 triangle
	rm = color Red . (!! 4) . iterate half . (!! 7) $ iterate rot45 triangle
	ws = color White . (!! 5) $ iterate half triangle
	rs = color Red . (!! 5) . iterate half . (!! 4) $ iterate rot45 triangle
	rec = (!! 2) . iterate half $ pictureLeft (n - 1)
	in
	br `overlap` left' wm `overlap` left' rm `overlap`
	(!! 3) (iterate up $ iterate left ws !! 3) `overlap`
	up (iterate left rs !! 3) `overlap`
	down (iterate left ws !! 3) `overlap`
	(!! 3) (iterate down $ iterate left rs !! 3) `overlap`
	(!! 3) (iterate up $ iterate left rec !! 3) `overlap`
	up (iterate left rec !! 3) `overlap`
	down (iterate left rec !! 3) `overlap`
	(!! 3) (iterate down $ iterate left rec !! 3)

triangle :: Picture
triangle = Picture 1 \cr -> do
	cairoMoveTo cr 0 0
	cairoLineTo cr 1 0
	cairoLineTo cr (1 / 2) (1 / 2)
	cairoFill cr

empty :: Picture
empty = Picture 0 $ const $ pure ()

left :: Picture -> Picture
left (Picture sz a) = Picture sz \cr -> local cr
	$ cairoTranslate cr (- sz / 2) 0 >> a cr

up :: Picture -> Picture
up (Picture sz a) = Picture sz \cr -> local cr
	$ cairoTranslate cr 0 (sz / 2) >> a cr

down :: Picture -> Picture
down (Picture sz a) = Picture sz \cr -> local cr
	$ cairoTranslate cr 0 (- sz / 2) >> a cr

left' :: Picture -> Picture
left' (Picture sz a) = Picture sz \cr -> local cr do
	cairoTranslate cr (- sz / sqrt 2) 0
	a cr

half :: Picture -> Picture
half (Picture sz a) = Picture (sz / sqrt 2) \cr -> local cr do
	cairoTranslate cr (1 / 2) (1 / 2)
	cairoScale cr (1 / sqrt 2) (1 / sqrt 2)
	cairoTranslate cr (- 1 / 2) (- 1 / 2)
	a cr

rot45 :: Picture -> Picture
rot45 (Picture sz a) = Picture sz \cr -> local cr do
	cairoTranslate cr (1 / 2) (1 / 2)
	cairoRotate cr (pi / 4)
	cairoTranslate cr (- 1 / 2) (- 1 / 2)
	a cr

overlap :: Picture -> Picture -> Picture
overlap (Picture sza a) (Picture szb b) = Picture (max sza szb) \cr -> a cr >> b cr

data Color = Red | Brown | White deriving Show

color :: Color -> Picture -> Picture
color clr (Picture sz a) = Picture sz \cr -> local cr do
	cairoSetSourceRgb cr $ getColor clr
	a cr
	where
	getColor = \case
		Red -> fromJust $ rgbDouble 0.7 0.2 0.1
		Brown -> fromJust $ rgbDouble 0.6 0.4 0.1
		White -> fromJust $ rgbDouble 0.8 0.8 0.8

local :: PrimMonad m => CairoT r (PrimState m) -> m a -> m a
local cr a = cairoSave cr >> a <* cairoRestore cr

main :: IO ()
main = drawPicture "fishPict.png" result

data Picture = Picture {
	pictureSize :: CDouble,
	pictureDraw ::
		(forall r m . PrimMonad m => CairoT r (PrimState m) -> m ()) }

drawPicture :: FilePath -> Picture -> IO ()
drawPicture fp (Picture _ act) = either error (writeArgb32 fp) $ runST do
	sfc0 <- cairoImageSurfaceCreate CairoFormatArgb32 700 700
	cr <- cairoCreate sfc0

	cairoSetMatrix cr =<< cairoMatrixNew 600 0 0 (- 600) 50 650

	act cr

	(<$> cairoImageSurfaceGetCairoImage sfc0) \case
		CairoImageArgb32 i -> Right i; _ -> Left "image format error"

writeArgb32 :: FilePath -> Argb32 -> IO ()
writeArgb32 fp = writePng fp . cairoArgb32ToJuicyRGBA8

scale :: CDouble -> Picture -> Picture
scale r (Picture s a) = Picture (r * s) \cr -> do
	m <- cairoGetMatrix cr
	cairoScale cr r r
	a cr
	cairoSetMatrix cr m

translate :: CDouble -> CDouble -> Picture -> Picture
translate dx dy (Picture sz a) = Picture sz \cr -> do
	m <- cairoGetMatrix cr
	cairoTranslate cr dx dy
	a cr
	cairoSetMatrix cr m
