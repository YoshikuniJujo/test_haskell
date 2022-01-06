{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Swizzle where

import GHC.Generics

class Swizzle1 a where
	type X a
	x :: a -> X a
	xx :: a -> (X a, X a)

	default x :: (Generic a, GSwizzle1 (Rep a), X a ~ GX (Rep a)) => a -> X a
	x = gx . from
	default xx :: (Generic a, GSwizzle1 (Rep a), X a ~ GX (Rep a)) => a -> (X a, X a)
	xx = gxx . from

class GSwizzle1 f where
	type GX f
	gx :: f a -> GX f
	gxx :: f a -> (GX f, GX f)

instance GSwizzle1 a => GSwizzle1 (M1 i c a) where
	type GX (M1 i c a) = GX a
	gx (M1 a) = gx a
	gxx (M1 a) = gxx a

instance GSwizzle1 (K1 i a) where
	type GX (K1 i a) = a
	gx (K1 a) = a
	gxx (K1 a) = (a, a)

instance GSwizzle1 a => GSwizzle1 (a :*: _b) where
	type GX (a :*: _b) = GX a
	gx (x_ :*: _) = gx x_
	gxx (x_ :*: _) = (gx x_, gx x_)

instance Swizzle1 (x, y) where
	type X (x, y) = x

class Swizzle1 a => Swizzle2 a where
	type Y a
	y :: a -> Y a
	xy :: a -> (X a, Y a)
	yx :: a -> (Y a, X a)
	yy :: a -> (Y a, Y a)

	default y :: (Generic a, GSwizzle2 (Rep a), Y a ~ GY (Rep a)) => a -> Y a
	y = gy . from
	default xy :: (Generic a, GSwizzle2 (Rep a), X a ~ GX (Rep a), Y a ~ GY (Rep a)) => a -> (X a, Y a)
	xy = gxy . from
	default yx :: (Generic a, GSwizzle2 (Rep a), X a ~ GX (Rep a), Y a ~ GY (Rep a)) => a -> (Y a, X a)
	yx = gyx . from
	default yy :: (Generic a, GSwizzle2 (Rep a), Y a ~ GY (Rep a)) => a -> (Y a, Y a)
	yy = gyy . from

class GSwizzle1 f => GSwizzle2 f where
	type GY f
	gy :: f a -> GY f
	gxy :: f a -> (GX f, GY f)
	gyx :: f a -> (GY f, GX f)
	gyy :: f a -> (GY f, GY f)

instance GSwizzle2 a => GSwizzle2 (M1 i c a) where
	type GY (M1 i c a) = GY a
	gy (M1 a) = gy a
	gxy (M1 a) = gxy a
	gyx (M1 a) = gyx a
	gyy (M1 a) = gyy a

instance (GSwizzle1 a, GSwizzle1 b) => GSwizzle2 (a :*: b) where
	type GY (a :*: b) = GX b
	gy (_a :*: b) = gx b
	gxy (a :*: b) = (gx a, gx b)
	gyx (a :*: b) = (gx b, gx a)
	gyy (_a :*: b) = (gx b, gx b)

instance Swizzle2 (x, y) where
	type Y (x, y) = y
