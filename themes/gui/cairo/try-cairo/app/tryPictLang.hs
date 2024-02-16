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
import Graphics.Cairo.Drawing.CairoT.Setting

import Fish

result :: Picture
result =
	pictureLeft Brown Red White 7 `overlap`
	(iterate rot45 (pictureLeft Red Brown White 7) !! 2) `overlap`
	(iterate rot45 (pictureLeft Brown Red White 7) !! 4) `overlap`
	(iterate rot45 (pictureLeft Red Brown White 7) !! 6)

pictureLeft :: Color -> Color -> Color -> Int -> Picture
pictureLeft c1 c2 c3 n = let
	rs = color c2 . (!! 5) . iterate half . (!! 4) $ iterate rot45 triangle
	in
	pictureLeftUp c1 c2 c3 n `overlap`
	(!! 3) (iterate down $ iterate left rs !! 3) `overlap`
	(!! 3) (iterate down $ iterate left (iterate half (pictureLeftDown c1 c2 c3 (n - 1)) !! 2) !! 3)
	

pictureLeftUp :: Color -> Color -> Color -> Int -> Picture
pictureLeftUp _ _ _ n | n < 1 = empty
pictureLeftUp c1 c2 c3 n = let
	foo = (!! 2) . iterate half . pictureLeftUp c1 c2 c3 $ n - 1
	ws = color c3 . (!! 5) $ iterate half triangle
	in
	(!! 3) (iterate up $ iterate left foo !! 3) `overlap`
	(!! 3) (iterate up $ iterate left ws !! 3) `overlap`
	pictureLeft1 c1 c2 c3 n

pictureLeftDown :: Color -> Color -> Color -> Int -> Picture
pictureLeftDown _ _ _ n | n < 1 = empty
pictureLeftDown c1 c2 c3 n = let
	foo = (!! 2) . iterate half . pictureLeftDown c1 c2 c3 $ n - 1
	rs = color c2 . (!! 5) . iterate half . (!! 4) $ iterate rot45 triangle
	in
	pictureLeft1 c1 c2 c3 n `overlap`
	(!! 3) (iterate down $ iterate left rs !! 3) `overlap`
	(!! 3) (iterate down $ iterate left foo !! 3)

pictureLeft1 :: Color -> Color -> Color -> Int -> Picture
pictureLeft1 _ _ _ n | n < 1 = empty
pictureLeft1 c1 c2 c3 n = let
	br = color c1
		. (!! 3) . iterate half . (!! 6) $ iterate rot45 triangle
	wm = color c3
		. (!! 4) . iterate half . (!! 5) $ iterate rot45 $ flipX triangle
	rm = color c2 . (!! 4) . iterate half . (!! 7) $ iterate rot45 $ flipX triangle
	ws = color c3 . (!! 5) $ iterate half triangle
	rs = color c2 . (!! 5) . iterate half . (!! 4) $ iterate rot45 triangle
	rec = (!! 2) . iterate half $ pictureLeft1 c1 c2 c3 (n - 1)
	in
	br `overlap` left' wm `overlap` left' rm `overlap`
--	(!! 3) (iterate up $ iterate left ws !! 3) `overlap`
	up (iterate left rs !! 3) `overlap`
	down (iterate left ws !! 3) `overlap`
--	(!! 3) (iterate down $ iterate left rs !! 3) `overlap`
{-	(!! 3) (iterate up $ iterate left rec !! 3) `overlap` -}
	up (iterate left rec !! 3) `overlap`
	down (iterate left rec !! 3) -- `overlap`
{-	(!! 3) (iterate down $ iterate left rec !! 3) -}

triangle :: Picture
triangle = flipX $ Picture 1 \cr clr -> do
	uncurry (cairoMoveTo cr) $ head fish
	uncurry (cairoLineTo cr) `mapM_` tail fish
	cairoClosePath cr
	cairoFill cr
	cairoSetSourceRgb cr $ getColor if clr /= White then White else Brown
		
--	cairoRectangle cr (4 / 5) (1 / 10) (1 / 20) (1 / 20)
	cairoMoveTo cr (4 / 5 + 1 / 20) (1 / 10 + 1 / 80)
	cairoLineTo cr (4 / 5 + 1 / 20) (1 / 10 + 1 / 20)
	cairoLineTo cr (4 / 5 + 9 / 80) (1 / 10)
	cairoClosePath cr
	cairoMoveTo cr (4 / 5 + 1 / 20) (1 / 30 + 1 / 80)
	cairoLineTo cr (4 / 5 + 1 / 20) (1 / 30 + 1 / 20)
	cairoLineTo cr (4 / 5 + 9 / 80) (1 / 30)
	cairoClosePath cr

	cairoMoveTo cr (4 / 20) (7 / 80)
	cairoLineTo cr (2 / 5) (12 / 80)
	cairoLineTo cr (4 / 5) (1 / 30 + 3 / 40)

	cairoMoveTo cr (2 / 5) (16 / 160)
	cairoLineTo cr (15 / 20) (1 / 40)

	cairoMoveTo cr (10 / 20) (5 / 20)
	cairoLineTo cr (15 / 20) (7 / 30)

	cairoMoveTo cr (20 / 40) (2 / 60)
	cairoLineTo cr (21 / 40) (3 / 60)

	cairoMoveTo cr (11 / 20) 0
	cairoLineTo cr (12 / 20) (2 / 60)

	cairoMoveTo cr (25 / 40) (- 2 / 120)
	cairoLineTo cr (27 / 40) (2 / 120)

	cairoMoveTo cr (11 / 20) (11 / 40)
	cairoLineTo cr (11 / 20) (17 / 40)

	cairoMoveTo cr (25 / 40) (11 / 40)
	cairoLineTo cr (25 / 40) (17 / 40)

	cairoMoveTo cr (28 / 40) (11 / 40)
	cairoLineTo cr (28 / 40) (17 / 40)

	cairoSet cr $ LineWidth (1 / 100)
	cairoStroke cr
	cairoNewPath cr
{-
triangle = Picture 1 \cr -> do
	cairoMoveTo cr 0 0
	cairoLineTo cr 1 0
	cairoLineTo cr (1 / 2) (1 / 2)
	cairoFill cr
	-}

empty :: Picture
empty = Picture 0 $ const . const $ pure ()

left :: Picture -> Picture
left (Picture sz a) = Picture sz \cr clr -> local cr
	$ cairoTranslate cr (- sz / 2) 0 >> a cr clr

up :: Picture -> Picture
up (Picture sz a) = Picture sz \cr clr -> local cr
	$ cairoTranslate cr 0 (sz / 2) >> a cr clr

down :: Picture -> Picture
down (Picture sz a) = Picture sz \cr clr -> local cr
	$ cairoTranslate cr 0 (- sz / 2) >> a cr clr

left' :: Picture -> Picture
left' (Picture sz a) = Picture sz \cr clr -> local cr do
	cairoTranslate cr (- sz / sqrt 2) 0
	a cr clr

half :: Picture -> Picture
half (Picture sz a) = Picture (sz / sqrt 2) \cr clr -> local cr do
	cairoTranslate cr (1 / 2) (1 / 2)
	cairoScale cr (1 / sqrt 2) (1 / sqrt 2)
	cairoTranslate cr (- 1 / 2) (- 1 / 2)
	a cr clr

rot45 :: Picture -> Picture
rot45 (Picture sz a) = Picture sz \cr clr -> local cr do
	cairoTranslate cr (1 / 2) (1 / 2)
	cairoRotate cr (pi / 4)
	cairoTranslate cr (- 1 / 2) (- 1 / 2)
	a cr clr

flipX :: Picture -> Picture
flipX (Picture sz a) = Picture sz \cr clr -> local cr do
	cairoTransform cr =<< cairoMatrixNew (- 1) 0 0 1 sz 0
	a cr clr

overlap :: Picture -> Picture -> Picture
overlap (Picture sza a) (Picture szb b) = Picture (max sza szb) \cr clr -> a cr clr >> b cr clr

data Color = Red | Brown | White deriving (Show, Eq)

color :: Color -> Picture -> Picture
color clr (Picture sz a) = Picture sz \cr _ -> local cr do
	cairoSetSourceRgb cr $ getColor clr
	a cr clr
	where

getColor = \case
	Red -> fromJust $ rgbDouble 0.7 0.2 0.1
	Brown -> fromJust $ rgbDouble 0.6 0.4 0.1
	White -> fromJust $ rgbDouble 0.5 0.5 0.3

local :: PrimMonad m => CairoT r (PrimState m) -> m a -> m a
local cr a = cairoSave cr >> a <* cairoRestore cr

main :: IO ()
main = drawPicture "fishPict.png" result

data Picture = Picture {
	pictureSize :: CDouble,
	pictureDraw :: (forall r m . PrimMonad m =>
		CairoT r (PrimState m) -> Color -> m ()) }

drawPicture :: FilePath -> Picture -> IO ()
drawPicture fp (Picture _ act) = either error (writeArgb32 fp) $ runST do
	sfc0 <- cairoImageSurfaceCreate CairoFormatArgb32 900 900
	cr <- cairoCreate sfc0

	cairoSetMatrix cr =<< cairoMatrixNew 800 0 0 (- 800) 50 850

	act cr White

	(<$> cairoImageSurfaceGetCairoImage sfc0) \case
		CairoImageArgb32 i -> Right i; _ -> Left "image format error"

writeArgb32 :: FilePath -> Argb32 -> IO ()
writeArgb32 fp = writePng fp . cairoArgb32ToJuicyRGBA8

scale :: CDouble -> Picture -> Picture
scale r (Picture s a) = Picture (r * s) \cr clr -> do
	m <- cairoGetMatrix cr
	cairoScale cr r r
	a cr clr
	cairoSetMatrix cr m

translate :: CDouble -> CDouble -> Picture -> Picture
translate dx dy (Picture sz a) = Picture sz \cr clr -> do
	m <- cairoGetMatrix cr
	cairoTranslate cr dx dy
	a cr clr
	cairoSetMatrix cr m
