{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Fish (fish) where

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
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Drawing.Transformations
import Graphics.Cairo.Surfaces.ImageSurfaces

main :: IO ()
main = either error (writeArgb32 "fish_halfway.png") $ runST draw

draw :: PrimMonad m => m (Either String Argb32)
draw = do
	sfc0 <- cairoImageSurfaceCreate CairoFormatArgb32 700 700
	cr <- cairoCreate sfc0

	cairoTranslate cr 350 350
	left cr 1 100 0 0
--	left cr 7 100 0 0
--	right cr 7 100 0 0
--	up cr 7 100 0 0
--	down cr 7 100 0 0

	(<$> cairoImageSurfaceGetCairoImage sfc0) \case
		CairoImageArgb32 i -> Right i; _ -> Left "image format error"

triangle :: PrimMonad m => CairoT r (PrimState m) ->
	CDouble -> CDouble -> CDouble -> CDouble -> m ()
-- triangle = polygon [(0, 0), (1, 0), (1 / 2, 1 / 2)]
triangle = polygon fish

fish :: [(CDouble, CDouble)]
fish = mkFish [(0, 0), (3, 0.8), (6, 0.6), (7, 2), (9.5, 2.5), (13, 0)]

data Line = Line {
	lineX1 :: CDouble,
	lineY1 :: CDouble,
	lineX2 :: CDouble,
	lineY2 :: CDouble } deriving Show

data Line' = Line' {
	lineX1' :: CDouble,
	lineY1' :: CDouble,
	lineUnit :: (CDouble, CDouble),
	lineLength :: CDouble } deriving Show

convert :: Line -> Line'
convert (Line x1 y1 x2 y2) =
	Line' x1 y1 (xd / d, yd / d) d
	where
	xd = x2 - x1; yd = y2 - y1
	d = sqrt $ xd ^ 2 + yd ^ 2

toFlat :: Line' -> (CDouble, CDouble) -> (CDouble, CDouble)
toFlat (Line' x1 y1 (xu, yu) d) (x, y) = (x'' / d, y'' / d)
	where
	x' = x - x1; y' = y - y1
	x'' = x' * xu + y' * yu
	y'' = - x' * yu + y' * xu

fromFlat :: Line' -> (CDouble, CDouble) -> (CDouble, CDouble)
fromFlat (Line' x1 y1 (xu, yu) d) (x, y) = (x'' + x1, y'' + y1)
	where
	x' = x * d; y' = y * d
	x'' = x' * xu - y' * yu
	y'' = x' * yu + y' * xu

lineToLine :: Line -> Line -> (CDouble, CDouble) -> (CDouble, CDouble)
lineToLine l1 l2 = fromFlat (convert l2) . toFlat (convert l1)

-- data Line' = Line' 

flipY :: (CDouble, CDouble) -> (CDouble, CDouble)
flipY (x, y) = (x, - y)

mkFish :: [(CDouble, CDouble)] -> [(CDouble, CDouble)]
mkFish ps =
	(fromFlat (convert $ Line 0 0 (1 / 2) 0) <$> us) ++
	reverse (fromFlat (convert $ Line 1 0 (1 / 2) 0) <$> us) ++
	(fromFlat (convert $ Line 1 0 (1 / 2) (1 / 2)) <$> fus) ++
	reverse (fromFlat (convert $ Line 0 0 (1 / 2) (1 / 2)) <$> fus)
	where
	(xa, ya) = head ps
	(xb, yb) = last ps
	us = toFlat (convert $ Line xa ya xb yb) <$> ps
	fus = flipY <$> us

polygon :: PrimMonad m => [(CDouble, CDouble)] -> CairoT r (PrimState m) ->
	CDouble -> CDouble -> CDouble -> CDouble -> m ()
polygon ps cr x1 y1 x2 y2 = do
	uncurry (cairoMoveTo cr) p0
	uncurry (cairoLineTo cr) `mapM_` ps'
	cairoFill cr
	where
	p0 : ps' = tr <$> ps
	tr (x, y) = (a * x + b * y + x1, b * x - a * y + y1)
	a = x2 - x1; b = y2 - y1

data Color = Red | Brown | White deriving Show

setColor :: PrimMonad m => CairoT r (PrimState m) -> Color -> m ()
setColor cr = cairoSetSourceRgb cr . getColor
	where
	getColor Red = fromJust $ rgbDouble 0.7 0.2 0.1
	getColor Brown = fromJust $ rgbDouble 0.6 0.6 0.1
	getColor White = fromJust $ rgbDouble 0.8 0.8 0.8

left, right, up, down :: PrimMonad m =>
	CairoT r (PrimState m) -> Int -> CDouble -> CDouble -> CDouble -> m ()
left = make tr Brown White Red
	where tr cr (x1, y1) (x2, y2) = triangle cr x1 y1 x2 y2

right = make tr Brown White Red
	where tr cr (x1, y1) (x2, y2) = triangle cr (- x1) (- y1) (- x2) (- y2)

up = make tr Red White Brown
	where tr cr (x1, y1) (x2, y2) = triangle cr (- y1) x1 (- y2) x2

down = make tr Red White Brown
	where tr cr (x1, y1) (x2, y2) = triangle cr y1 (- x1) y2 (- x2)

make :: PrimMonad m =>
	(CairoT r (PrimState m) ->
		(CDouble, CDouble) -> (CDouble, CDouble) -> m ())->
	Color -> Color -> Color ->
	CairoT r (PrimState m) -> Int -> CDouble -> CDouble -> CDouble -> m ()
make _ _ _ _ _ n _ _ _ | n < 0 = pure ()
make tr c1 c2 c3 cr n sz x y = do
	setColor cr c1; tr cr p2 p5
	{-
	setColor cr c2; tr cr p1 p2
	setColor cr c3; tr cr p2 p1
	setColor cr c2; tr cr p2 p3
	setColor cr c3; tr cr p3 p5
	setColor cr c2; tr cr p4 p5
	setColor cr c3; tr cr p5 p4
	make tr c1 c2 c3 cr (n - 1) (sz / 2) (x - sz * 3 / 2) (y - sz - sz / 2)
	make tr c1 c2 c3 cr (n - 1) (sz / 2) (x - sz - sz / 2) (y - sz / 2)
	make tr c1 c2 c3 cr (n - 1) (sz / 2) (x - sz - sz / 2) (y + sz / 2)
	make tr c1 c2 c3 cr (n - 1) (sz / 2) (x - sz * 3 / 2) (y + sz + sz / 2)
	-}
	where
	p1 = (x - sz * 2, y - sz); p2 = (x - sz, y - sz)
	p3 = (x - sz * 2, y)
	p4 = (x - sz * 2, y + sz); p5 = (x - sz, y + sz)

writeArgb32 :: FilePath -> Argb32 -> IO ()
writeArgb32 fp = writePng fp . cairoArgb32ToJuicyRGBA8
