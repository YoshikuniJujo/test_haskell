{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphic.Shape where

data Rectangle
	= RectLURB_ Double Double Double Double
	| RectLUWH_ Double Double Double Double
	deriving Show

right, bottom :: Rectangle -> Double
right = \case RectLURB_ _ _ r _ -> r; RectLUWH_ l _ w _ -> l + w
bottom = \case RectLURB_ _ _ _ b -> b; RectLUWH_ _ u _ h -> u + h

-- pattern RectLURB :: Double -> Double -> Double -> Double -> Rectangle
-- pattern RectLURB 
