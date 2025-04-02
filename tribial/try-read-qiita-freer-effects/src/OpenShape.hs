{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module OpenShape where

data SomeShape = forall s . Shape s => SomeShape s

class Shape s where shapeArea :: s -> Double

area :: SomeShape -> Double
area (SomeShape s) = shapeArea s

data Rectangle = Rectangle (Double, Double) Double Double

instance Shape Rectangle where
	shapeArea (Rectangle _ w h) = w * h

data Circle = Circle (Double, Double) Double

instance Shape Circle where
	shapeArea (Circle _ r) = r * r * pi

areas :: [SomeShape] -> [Double]
areas = map area
