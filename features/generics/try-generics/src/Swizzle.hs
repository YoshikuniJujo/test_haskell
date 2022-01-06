{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Swizzle where

import GHC.Generics

class Swizzle2 a where
	type X a
	type Y a
	x :: a -> X a
	y :: a -> Y a
	xx :: a -> (X a, X a)
	xy :: a -> (X a, Y a)
	yx :: a -> (Y a, X a)
	yy :: a -> (Y a, Y a)

	default x :: (Generic a, GSwizzle2 (Rep a), X a ~ GX (Rep a)) => a -> X a
	x = gx . from

instance Swizzle2 (x, y) where
	type X (x, y) = x
	type Y (x, y) = y
	x (x_, _y) = x_
	y (_x, y_) = y_
	xx (x_, _y) = (x_, x_)
	xy (x_, y_) = (x_, y_)
	yx (x_, y_) = (y_, x_)
	yy (_x, y_) = (y_, y_)

class GSwizzle2 f where
	type GX f
	type GY f
	gx :: f a -> GX f
	gy :: f a -> GY f
	gxx :: f a -> (GX f, GX f)
	gxy :: f a -> (GX f, GY f)
	gyx :: f a -> (GY f, GX f)
	gyy :: f a -> (GY f, GY f)

{-
instance {-# OVERLAPPABLE #-} GSwizzle2 (x :*: y) where
--	type GX (Rep x _ :*: Rep y _) = x
--	type GY (Rep x _ :*: Rep y _) = y
	gx (K1 x_ :*: _) = x_
	gy (_ :*: K1 y_) = y_
	gxx (K1 x_ :*: _) = (x_, x_)
	gxy (K1 x_ :*: K1 y_) = (x_, y_)
	gyx (K1 x_ :*: K1 y_) = (y_, x_)
	gyy (_ :*: K1 y_) = (y_, y_)

instance GSwizzle2 a => GSwizzle2 (M1 i c a) where
	type GX (M1 i c a) = GX a
	gx (M1 a) = gx a
	-}

data Point = Point Double Double deriving (Show, Generic)

-- instance Swizzle2 Point
