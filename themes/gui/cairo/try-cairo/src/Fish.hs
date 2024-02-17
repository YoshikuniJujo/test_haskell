{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Fish (fish) where

import Foreign.C.Types

fish :: [(CDouble, CDouble)]
fish = mkFish [(0, 0), (3, 0.8), (6, 0.6), (7, 2), (9.5, 2.5), (13, 0)]

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

flipY :: (CDouble, CDouble) -> (CDouble, CDouble)
flipY (x, y) = (x, - y)
