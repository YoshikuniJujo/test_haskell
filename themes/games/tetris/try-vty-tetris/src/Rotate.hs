{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Rotate (Center(Grid), Pos, PosR, nearestCenter, rotatePosL, rotatePosR) where

{-

	.----> x
	|
	|
	V
	y
	

-}

type Pos = (Int, Int)
type PosR = (Rational, Rational)
data Center = Grid Int Int | Middle Int Int deriving (Eq, Show)

nearestCenter :: PosR -> Center
nearestCenter (x, y)
	| 0.5 < fs && fs < 1.5 && - 0.5 < fd && fd < 0.5 = Middle nx ny
	| otherwise = Grid (round x) (round y)
	where
	fs = fx + fy
	fd = fx - fy
	(nx, fx) = properFraction x
	(ny, fy) = properFraction y

rotatePosL, rotatePosR :: Center -> Pos -> Pos
rotatePosL (Grid cx cy) (x, y) = ((y - cy) + cx, - (x - cx) + cy)
rotatePosL (Middle cx cy) (x, y) = ((y - cy) + cx, - (x - cx) + cy + 1)

rotatePosR (Grid cx cy) (x, y) = (- (y - cy) + cx, (x - cx) + cy)
rotatePosR (Middle cx cy) (x, y) = (- (y - cy) + cx + 1, (x - cx) + cy)
