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
result = filled 7

filled :: Int -> Picture
filled n = let p f = f filledLeft Brown Red White n in
	p id `overlap` (2 `times` rot45) (p flip) `overlap`
	(4 `times` rot45) (p id) `overlap` (6 `times` rot45) (p flip)

filledLeft :: Color -> Color -> Color -> Int -> Picture
filledLeft c1 c2 c3 n =
	filledLeftUp c1 c2 c3 n `overlap`
	(3 `times` leftDown)
		((5 `times` half) . (4 `times` rot45) $ color c2 fish) `overlap`
	(3 `times` leftDown)
		((2 `times` half) . filledLeftDown c1 c2 c3 $ n - 1)

filledLeftUp :: Color -> Color -> Color -> Int -> Picture
filledLeftUp _ _ _ n | n < 1 = empty
filledLeftUp c1 c2 c3 n =
	(3 `times` leftUp)
		((2 `times` half) . filledLeftUp c1 c2 c3 $ n - 1) `overlap`
	(3 `times` leftUp) ((5 `times` half) $ color c3 fish) `overlap`
	filledLeft1 c1 c2 c3 n

filledLeftDown :: Color -> Color -> Color -> Int -> Picture
filledLeftDown _ _ _ n | n < 1 = empty
filledLeftDown c1 c2 c3 n =
	filledLeft1 c1 c2 c3 n `overlap`
	(3 `times` leftDown)
		((5 `times` half) . (4 `times` rot45) $ color c2 fish) `overlap`
	(3 `times` leftDown)
		((2 `times` half) . filledLeftDown c1 c2 c3 $ n - 1)

filledLeft1 :: Color -> Color -> Color -> Int -> Picture
filledLeft1 _ _ _ n | n < 1 = empty
filledLeft1 c1 c2 c3 n =
	(3 `times` half) ((6 `times` rot45) $ color c1 fish) `overlap`
	left ((4 `times` half)
		. (5 `times` rot45) . color c3 $ flipX fish) `overlap`
	left ((4 `times` half)
		. (7 `times` rot45) . color c2 $ flipX fish) `overlap`
	up ((3 `times` left) . (5 `times` half)
		. (4 `times` rot45) $ color c2 fish) `overlap`
	down ((3 `times` left) . (5 `times` half) $ color c3 fish) `overlap`
	up rec `overlap` down rec
	where
	rec = (3 `times` left) . (2 `times` half) . filledLeft1 c1 c2 c3 $ n - 1

fish :: Picture
fish = flipX $ shape 1 fishShape `overlap` pattern White Brown fishPattern

--------------------------------------------------
-- FISH SHAPE
--------------------------------------------------

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
-- FISH PATTERN
--------------------------------------------------

fishPattern :: [Poly]
fishPattern = [
	Polygon (
		(64 / 80 + 4 / 80, 8 / 80 + 1 / 80) NE.:| [
		(64 / 80 + 4 / 80, 8 / 80 + 4 / 80),
		(64 / 80 + 9 / 80, 8 / 80) ] ),
	Polygon (
		(64 / 80 + 4 / 80, 11 / 240) NE.:| [
		(64 / 80 + 4 / 80, 20 / 240),
		(64 / 80 + 9 / 80, 8 / 240) ] ),
	Polyline (
		(48 / 240, 21 / 240) NE.:| [
		(96 / 240, 39 / 240), (192 / 240, 8 / 240 + 18 / 240) ] ),
	Polyline ((96 / 240, 24 / 240) NE.:| [(180 / 240, 6 / 240)]),
	Polyline ((120 / 240, 60 / 240) NE.:| [(180 / 240, 56 / 240)]),
	Polyline ((120 / 240, 8 / 240) NE.:| [(126 / 240, 12 / 240)]),
	Polyline ((132 / 240, 0) NE.:| [(144 / 240, 8 / 240)]),
	Polyline ((150 / 240, - 4 / 240) NE.:| [(162 / 240, 4 / 240)]),
	Polyline ((132 / 240, 66 / 240) NE.:| [(132 / 240, 102 / 240)]),
	Polyline ((150 / 240, 66/ 240) NE.:| [(150 / 240, 102 / 240)]),
	Polyline ((168 / 240, 66 / 240) NE.:| [(168 / 240, 102 / 240)]) ]

--------------------------------------------------
-- PICTURE LANGUAGE
--------------------------------------------------

-- TRANSFORM

leftUp, leftDown :: Picture -> Picture
leftUp = up . left; leftDown = down . left

left :: Picture -> Picture
left (Picture sz sz' a) = Picture sz sz' \cr clr -> local cr
	$ cairoTranslate cr (- sz / 2) 0 >> a cr clr

up :: Picture -> Picture
up (Picture sz sz' a) = Picture sz sz' \cr clr -> local cr
	$ cairoTranslate cr 0 (sz / 2) >> a cr clr

down :: Picture -> Picture
down (Picture sz sz' a) = Picture sz sz' \cr clr -> local cr
	$ cairoTranslate cr 0 (- sz / 2) >> a cr clr

half :: Picture -> Picture
half (Picture sz sz' a) = Picture (sz / sqrt 2) (sz' / sqrt 2) \cr clr -> local cr do
	cairoTranslate cr (1 / 2) (1 / 2)
	cairoScale cr (1 / sqrt 2) (1 / sqrt 2)
	cairoTranslate cr (- 1 / 2) (- 1 / 2)
	a cr clr

rot45 :: Picture -> Picture
rot45 (Picture sz sz' a) = Picture sz' sz \cr clr -> local cr do
	cairoTranslate cr (1 / 2) (1 / 2)
	cairoRotate cr (pi / 4)
	cairoTranslate cr (- 1 / 2) (- 1 / 2)
	a cr clr

flipX :: Picture -> Picture
flipX (Picture sz sz' a) = Picture sz sz' \cr clr -> local cr do
	cairoTransform cr =<< cairoMatrixNew (- 1) 0 0 1 sz 0
	a cr clr

overlap :: Picture -> Picture -> Picture
overlap (Picture sza sza' a) (Picture szb szb' b) =
	Picture (max sza szb) (max sza' szb') \cr clr -> a cr clr >> b cr clr

times :: Int -> (a -> a) -> a -> a
times n f = (!! n) . iterate f

local :: PrimMonad m => CairoT r (PrimState m) -> m a -> m a
local cr a = cairoSave cr >> a <* cairoRestore cr

-- Color

color :: Color -> Picture -> Picture
color clr (Picture sz sz' a) = Picture sz sz' \cr _ -> local cr
	$ cairoSetSourceRgb cr (getColor clr) >> a cr clr

data Color = Red | Brown | White deriving (Show, Eq)

getColor :: Color -> Rgb CDouble
getColor = \case
	Red -> fromJust $ rgbDouble 0.7 0.2 0.1
	Brown -> fromJust $ rgbDouble 0.6 0.4 0.1
	White -> fromJust $ rgbDouble 0.5 0.5 0.3

-- Picture

data Picture = Picture {
	_pictureSize :: CDouble,
	_pictureSize2 :: CDouble,
	_pictureDraw :: (forall r m . PrimMonad m =>
		CairoT r (PrimState m) -> Color -> m ()) }

empty :: Picture
empty = Picture 0 0 $ const . const $ pure ()

shape :: CDouble -> NE.NonEmpty (CDouble, CDouble) -> Picture
shape sz sp = Picture sz (sz * sqrt 2) \cr _ -> local cr do
	uncurry (cairoMoveTo cr) $ NE.head fishShape
	uncurry (cairoLineTo cr) `mapM_` NE.tail sp
	cairoClosePath cr
	cairoSet cr $ LineWidth (1 / 400)
	cairoStrokePreserve cr
	cairoFill cr

pattern :: Color -> Color -> [Poly] -> Picture
pattern clr1 clr2 ps = Picture 1 (sqrt 2) \cr spclr -> do
	cairoSetSourceRgb cr $ getColor if spclr /= clr1 then clr1 else clr2
	poly cr `mapM_` ps
	cairoSet cr $ LineWidth (1 / 100)
	cairoStroke cr

data Poly
	= Polyline (NE.NonEmpty (CDouble, CDouble))
	| Polygon (NE.NonEmpty (CDouble, CDouble))
	deriving Show

poly :: PrimMonad m => CairoT r (PrimState m) -> Poly -> m ()
poly cr = \case
	Polyline (h NE.:| t) ->
		uncurry (cairoMoveTo cr) h >> uncurry (cairoLineTo cr) `mapM_` t
	Polygon (h NE.:| t) -> do
		uncurry (cairoMoveTo cr) h >> uncurry (cairoLineTo cr) `mapM_` t
		cairoClosePath cr

--------------------------------------------------
-- OUTPUT PICTURE
--------------------------------------------------

main :: IO ()
main = drawPicture "fishPict.png" result

drawPicture :: FilePath -> Picture -> IO ()
drawPicture fp (Picture _ _ act) = either error (writeArgb32 fp) $ runST do
	sfc <- cairoImageSurfaceCreate CairoFormatArgb32 900 900
	cr <- cairoCreate sfc
	cairoSetMatrix cr =<< cairoMatrixNew 800 0 0 (- 800) 50 850
	act cr White
	(<$> cairoImageSurfaceGetCairoImage sfc) \case
		CairoImageArgb32 i -> Right i; _ -> Left "image format error"

writeArgb32 :: FilePath -> Argb32 -> IO ()
writeArgb32 fp = writePng fp . cairoArgb32ToJuicyRGBA8