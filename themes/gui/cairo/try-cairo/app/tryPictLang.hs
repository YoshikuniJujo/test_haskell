{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Foreign.C.Types
import Control.Arrow ((&&&), second)
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Maybe
import Data.List.NonEmpty qualified as NE
import Data.Color
import Data.CairoImage.Internal
import Data.CairoContext
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoT.Setting
import Graphics.Cairo.Drawing.CairoT.SaveAndRestore
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Drawing.Transformations
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Utilities.CairoMatrixT

--------------------------------------------------
-- FILL BY FISH
--------------------------------------------------

result :: Picture
result = picture 7

picture :: Int -> Picture
picture n =
	pictureLeft Brown Red White n `overlap`
	(2 `times` rot45) (pictureLeft Red Brown White n) `overlap`
	(4 `times` rot45) (pictureLeft Brown Red White n) `overlap`
	(6 `times` rot45) (pictureLeft Red Brown White n)

pictureLeft :: Color -> Color -> Color -> Int -> Picture
pictureLeft c1 c2 c3 n =
	pictureLeftUp c1 c2 c3 n `overlap`
	(3 `times` leftDown)
		((5 `times` half) . (4 `times` rot45) $ color c2 fish) `overlap`
	(3 `times` leftDown)
		((2 `times` half) . pictureLeftDown c1 c2 c3 $ n - 1)

pictureLeftUp :: Color -> Color -> Color -> Int -> Picture
pictureLeftUp _ _ _ n | n < 1 = empty
pictureLeftUp c1 c2 c3 n =
	(3 `times` leftUp)
		((2 `times` half) . pictureLeftUp c1 c2 c3 $ n - 1) `overlap`
	(3 `times` leftUp) ((5 `times` half) $ color c3 fish) `overlap`
	pictureLeft1 c1 c2 c3 n

pictureLeftDown :: Color -> Color -> Color -> Int -> Picture
pictureLeftDown _ _ _ n | n < 1 = empty
pictureLeftDown c1 c2 c3 n =
	pictureLeft1 c1 c2 c3 n `overlap`
	(3 `times` leftDown)
		((5 `times` half) . (4 `times` rot45) $ color c2 fish) `overlap`
	(3 `times` leftDown)
		((2 `times` half) . pictureLeftDown c1 c2 c3 $ n - 1)

pictureLeft1 :: Color -> Color -> Color -> Int -> Picture
pictureLeft1 _ _ _ n | n < 1 = empty
pictureLeft1 c1 c2 c3 n =
	(3 `times` half) ((6 `times` rot45) $ color c1 fish) `overlap`
	left' ((4 `times` half)
		. (5 `times` rot45) . color c3 $ flipX fish) `overlap`
	left' ((4 `times` half)
		. (7 `times` rot45) . color c2 $ flipX fish) `overlap`
	up ((3 `times` left) . (5 `times` half)
		. (4 `times` rot45) $ color c2 fish) `overlap`
	down ((3 `times` left) . (5 `times` half) $ color c3 fish) `overlap`
	up rec `overlap` down rec
	where
	rec = (3 `times` left)
		. (2 `times` half) . pictureLeft1 c1 c2 c3 $ n - 1

fish :: Picture
fish = fishBackground `overlap` fishForeground

--------------------------------------------------
-- FISH BACKGROUND
--------------------------------------------------

fishBackground :: Picture
fishBackground = flipX $ Picture 1 \cr _ -> do
	uncurry (cairoMoveTo cr) $ NE.head fishShape
	uncurry (cairoLineTo cr) `mapM_` NE.tail fishShape
	cairoClosePath cr
	cairoSet cr $ LineWidth (1 / 400)
	cairoStrokePreserve cr
	cairoFill cr

-- fish shape

fishShape :: NE.NonEmpty (CDouble, CDouble)
fishShape =
	mkShape $ (0, 0) NE.:| [(30, 8), (60, 6), (70, 20), (95, 25), (130, 0)]

mkShape :: NE.NonEmpty (CDouble, CDouble) -> NE.NonEmpty (CDouble, CDouble)
mkShape ps@(NE.head &&& NE.last -> (h, l)) =
	(along (line a b) <$> u) <> (along (line c b) <$> NE.reverse u) <>
	(along (line c d) <$> u') <> (along (line a d) <$> NE.reverse u') where
	a = (0, 0); b = (1 / 2, 0); c = (1, 0); d = (1 / 2, 1 / 2)
	u = unit (line h l) <$> ps; u' = second negate <$> u

along :: Line -> (CDouble, CDouble) -> (CDouble, CDouble)
along (Line x0 y0 (rx, ry) d) (x, y) = (x'' + x0, y'' + y0) where
	x' = x * d; y' = y * d; x'' = x' * rx - y' * ry; y'' = x' * ry + y' * rx

unit :: Line -> (CDouble, CDouble) -> (CDouble, CDouble)
unit (Line x0 y0 (rx, ry) d) (x, y) = (x'' / d, y'' / d) where
	x' = x - x0; y' = y - y0
	x'' = x' * rx + y' * ry; y'' = - x' * ry + y' * rx

data Line = Line {
	lineX1 :: CDouble, lineY1 :: CDouble,
	lineUnit :: (CDouble, CDouble), lineLength :: CDouble } deriving Show

line :: (CDouble, CDouble) -> (CDouble, CDouble) -> Line
line (x1, y1) (x2, y2) = Line x1 y1 (xd / d, yd / d) d where
	xd = x2 - x1; yd = y2 - y1; d = sqrt $ xd ^ (2 :: Int) + yd ^ (2 :: Int)

--------------------------------------------------
-- FISH FOREGROUND
--------------------------------------------------

fishForeground :: Picture
fishForeground = flipX $ Picture 1 \cr clr -> do
	cairoSetSourceRgb cr $ getColor if clr /= White then White else Brown
		
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

--------------------------------------------------
-- PICTURE LANGUAGE
--------------------------------------------------

data Picture = Picture {
	_pictureSize :: CDouble,
	_pictureDraw :: (forall r m . PrimMonad m =>
		CairoT r (PrimState m) -> Color -> m ()) }

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

leftUp, leftDown :: Picture -> Picture
leftUp = up . left; leftDown = down . left

left' :: Picture -> Picture
left' (Picture sz a) = Picture sz \cr clr -> local cr
	$ cairoTranslate cr (- sz / sqrt 2) 0 >> a cr clr

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
overlap (Picture sza a) (Picture szb b) =
	Picture (max sza szb) \cr clr -> a cr clr >> b cr clr

data Color = Red | Brown | White deriving (Show, Eq)

color :: Color -> Picture -> Picture
color clr (Picture sz a) = Picture sz \cr _ -> local cr
	$ cairoSetSourceRgb cr (getColor clr) >> a cr clr

getColor :: Color -> Rgb CDouble
getColor = \case
	Red -> fromJust $ rgbDouble 0.7 0.2 0.1
	Brown -> fromJust $ rgbDouble 0.6 0.4 0.1
	White -> fromJust $ rgbDouble 0.5 0.5 0.3

local :: PrimMonad m => CairoT r (PrimState m) -> m a -> m a
local cr a = cairoSave cr >> a <* cairoRestore cr

times :: Int -> (a -> a) -> a -> a
times n f = (!! n) . iterate f

--------------------------------------------------
-- OUTPUT PICTURE
--------------------------------------------------

main :: IO ()
main = drawPicture "fishPict.png" result

drawPicture :: FilePath -> Picture -> IO ()
drawPicture fp (Picture _ act) = either error (writeArgb32 fp) $ runST do
	sfc <- cairoImageSurfaceCreate CairoFormatArgb32 900 900
	cr <- cairoCreate sfc
	cairoSetMatrix cr =<< cairoMatrixNew 800 0 0 (- 800) 50 850
	act cr White
	(<$> cairoImageSurfaceGetCairoImage sfc) \case
		CairoImageArgb32 i -> Right i; _ -> Left "image format error"

writeArgb32 :: FilePath -> Argb32 -> IO ()
writeArgb32 fp = writePng fp . cairoArgb32ToJuicyRGBA8
