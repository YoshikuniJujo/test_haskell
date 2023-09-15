{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphic.Shape (Rectangle, pattern RectLURB, pattern RectLUWH) where

import Control.Arrow ((&&&))

data Rectangle
	= RectLURB_ Double Double Double Double
	| RectLUWH_ Double Double Double Double
	deriving Show

left, upper :: Rectangle -> Double
left = \case RectLURB_ l _ _ _ -> l; RectLUWH_ l _ _ _ -> l;
upper = \case RectLURB_ _ u _ _ -> u; RectLUWH_ _ u _ _ -> u;

right, bottom :: Rectangle -> Double
right = \case RectLURB_ _ _ r _ -> r; RectLUWH_ l _ w _ -> l + w
bottom = \case RectLURB_ _ _ _ b -> b; RectLUWH_ _ u _ h -> u + h

width, height :: Rectangle -> Double
width = \case RectLURB_ l _ r _ -> r - l; RectLUWH_ _ _ w _ -> w
height = \case RectLURB_ _ u _ b -> b - u; RectLUWH_ _ _ _ h -> h

pattern RectLURB :: Double -> Double -> Double -> Double -> Rectangle
pattern RectLURB l u r b <-
	(((left &&& upper) &&& (right &&& bottom)) -> ((l, u), (r, b)))
	where RectLURB = RectLURB_

pattern RectLUWH :: Double -> Double -> Double -> Double -> Rectangle
pattern RectLUWH l u w h <-
	(((left &&& upper) &&& (width &&& height)) -> ((l, u), (w, h)))
