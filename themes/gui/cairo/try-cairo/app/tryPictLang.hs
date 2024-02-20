{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingStrategies, DeriveGeneric, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import GHC.Generics
import Foreign.C.Types
import Control.Arrow ((***), (&&&), second)
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Maybe
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Color
import Data.CairoImage.Internal
import Data.CairoContext
import Data.JuicyCairo
import System.Environment
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoT.Setting
import Graphics.Cairo.Drawing.CairoT.SaveAndRestore
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Drawing.Transformations
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Utilities.CairoMatrixT

import Dhall hiding (unit)

--------------------------------------------------
-- FILL BY FISH
--------------------------------------------------

result :: FishParams -> Picture
result fp = filled fish (color1 clrs) (color2 clrs) (color3 clrs) 7
	where
	fish = flipX $ shape 1 fishShape `overlap`
		pattern (patternColor1 pt) (patternColor2 pt) fishPattern'
	pt = fishPattern fp
	clrs = colors fp
	fishShape = mkShape . ((realToFrac *** realToFrac) <$>)
		. nonEmpty $ oneSide fp
	fishPattern' = (`scale` (1 / patternSize (fishPattern fp)))
		<$> patternBody (fishPattern fp)

filled :: Picture -> Color -> Color -> Color -> Int -> Picture
filled p0 c1 c2 c3 n = let p f = f (filledLeft p0) c1 c2 c3 n in
	p id `overlap` (2 `times` rot45) (p flip) `overlap`
	(4 `times` rot45) (p id) `overlap` (6 `times` rot45) (p flip)

filledLeft :: Picture -> Color -> Color -> Color -> Int -> Picture
filledLeft p c1 c2 c3 n =
	filledLeftUp p c1 c2 c3 n `overlap`
	(3 `times` leftDown)
		((5 `times` half) . (4 `times` rot45) $ color c2 p) `overlap`
	(3 `times` leftDown)
		((2 `times` half) . filledLeftDown p c1 c2 c3 $ n - 1)

filledLeftUp :: Picture -> Color -> Color -> Color -> Int -> Picture
filledLeftUp _ _ _ _ n | n < 1 = empty
filledLeftUp p c1 c2 c3 n =
	(3 `times` leftUp)
		((2 `times` half) . filledLeftUp p c1 c2 c3 $ n - 1) `overlap`
	(3 `times` leftUp) ((5 `times` half) $ color c3 p) `overlap`
	filledLeft1 p c1 c2 c3 n

filledLeftDown :: Picture -> Color -> Color -> Color -> Int -> Picture
filledLeftDown _ _ _ _ n | n < 1 = empty
filledLeftDown p c1 c2 c3 n =
	filledLeft1 p c1 c2 c3 n `overlap`
	(3 `times` leftDown)
		((5 `times` half) . (4 `times` rot45) $ color c2 p) `overlap`
	(3 `times` leftDown)
		((2 `times` half) . filledLeftDown p c1 c2 c3 $ n - 1)

filledLeft1 :: Picture -> Color -> Color -> Color -> Int -> Picture
filledLeft1 _ _ _ _ n | n < 1 = empty
filledLeft1 p c1 c2 c3 n =
	(3 `times` half) ((6 `times` rot45) $ color c1 p) `overlap`
	left ((4 `times` half)
		. (5 `times` rot45) . color c3 $ flipX p) `overlap`
	left ((4 `times` half)
		. (7 `times` rot45) . color c2 $ flipX p) `overlap`
	up ((3 `times` left) . (5 `times` half)
		. (4 `times` rot45) $ color c2 p) `overlap`
	down ((3 `times` left) . (5 `times` half) $ color c3 p) `overlap`
	up rec `overlap` down rec
	where
	rec = (3 `times` left) . (2 `times` half) . filledLeft1 p c1 c2 c3 $ n - 1

--------------------------------------------------
-- FISH SHAPE
--------------------------------------------------

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

--------------------------------------------------
-- FISH PARAMS
--------------------------------------------------

data FishParams = FishParams {
	oneSide :: NonEmpty (Double, Double),
	fishPattern :: Pattern,
	colors :: Colors }
	deriving (Show, Generic)

instance FromDhall FishParams

data Pattern = Pattern {
	patternColor1 :: Color,
	patternColor2 :: Color,
	patternSize :: Double,
	patternBody :: [Poly] }
	deriving (Show, Generic)

instance FromDhall Pattern

data Colors = Colors { color1 :: Color, color2 :: Color, color3 :: Color }
	deriving (Show, Generic)

instance FromDhall Colors

newtype NonEmpty a = NonEmpty { nonEmpty :: NE.NonEmpty a }
	deriving Show deriving newtype (Generic, Functor, Applicative, Monad)

instance FromDhall a => FromDhall (NonEmpty a)

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

data Color
	= White | Black | Red | Green | Yellow | Blue | Brown | Purple | Pink
	| Orange | Gray
	deriving (Show, Eq, Generic)

instance FromDhall Color

getColor :: Color -> Rgb CDouble
getColor = \case
	White -> fromJust $ rgbDouble 0.5 0.5 0.3
	Black -> fromJust $ rgbDouble 0.2 0.2 0.1
	Red -> fromJust $ rgbDouble 0.7 0.2 0.1
	Green -> fromJust $ rgbDouble 0.2 0.5 0.1
	Yellow -> fromJust $ rgbDouble 0.5 0.5 0.1
	Blue -> fromJust $ rgbDouble 0.2 0.2 0.4
	Brown -> fromJust $ rgbDouble 0.6 0.4 0.1
	Purple -> fromJust $ rgbDouble 0.6 0.1 0.3
	Pink -> fromJust $ rgbDouble 0.7 0.3 0.2
	Orange -> fromJust $ rgbDouble 0.7 0.4 0.2
	Gray -> fromJust $ rgbDouble 0.3 0.3 0.2

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
	uncurry (cairoMoveTo cr) $ NE.head sp
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
	= Polyline (NonEmpty (Double, Double))
	| Polygon (NonEmpty (Double, Double))
	deriving (Show, Generic)

instance FromDhall Poly

poly :: PrimMonad m => CairoT r (PrimState m) -> Poly -> m ()
poly cr = \case
	Polyline (NonEmpty (h NE.:| t)) -> do
		uncurry (cairoMoveTo cr) $ (realToFrac *** realToFrac) h
		(uncurry (cairoLineTo cr)
			. (realToFrac *** realToFrac)) `mapM_` t
	Polygon (NonEmpty (h NE.:| t)) -> do
		uncurry (cairoMoveTo cr) $ (realToFrac *** realToFrac) h
		(uncurry (cairoLineTo cr)
			. (realToFrac *** realToFrac)) `mapM_` t
		cairoClosePath cr

scale :: Poly -> Double -> Poly
scale (Polyline ps) s = Polyline $ ((* s) *** (* s)) <$> ps
scale (Polygon ps) s = Polygon $ ((* s) *** (* s)) <$> ps

--------------------------------------------------
-- OUTPUT PICTURE
--------------------------------------------------

main :: IO ()
main = do
	cfg <- (<$> getArgs) \case
		[] -> "./defaultFishParams"
		[fn] -> T.pack fn
		_ -> error "bad args"
	fp <- input auto cfg
	drawPicture "fishPict.png" (result fp)

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
