{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SwizzleOld where

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
	default y :: (Generic a, GSwizzle2 (Rep a), Y a ~ GY (Rep a)) => a -> Y a
	y = gy . from
	default xx :: (Generic a, GSwizzle2 (Rep a), X a ~ GX (Rep a)) => a -> (X a, X a)
	xx = gxx . from
	default xy :: (
		Generic a, GSwizzle2 (Rep a), X a ~ GX (Rep a), Y a ~ GY (Rep a)
		) => a -> (X a, Y a)
	xy = gxy . from
	default yx :: (
		Generic a, GSwizzle2 (Rep a), X a ~ GX (Rep a), Y a ~ GY (Rep a)
		) => a -> (Y a, X a)
	yx = gyx . from
	default yy :: (Generic a, GSwizzle2 (Rep a), Y a ~ GY (Rep a)) => a -> (Y a, Y a)
	yy = gyy . from

class GSwizzle2 f where
	type GX f
	type GY f
	gx :: f a -> GX f
	gy :: f a -> GY f
	gxx :: f a -> (GX f, GX f)
	gxy :: f a -> (GX f, GY f)
	gyx :: f a -> (GY f, GX f)
	gyy :: f a -> (GY f, GY f)

instance (One x, Swizzle1 y) => GSwizzle2 (x :*: y) where
	type GX (x :*: _) = OneType x
	type GY (_ :*: y) = HeadType y
	gx (x_ :*: _) = one x_
	gy (_ :*: y_) = hd y_
	gxx (x_ :*: _) = (one x_, one x_)
	gxy (x_ :*: y_) = (one x_, hd y_)
	gyx (x_ :*: y_) = (hd y_, one x_)
	gyy (_ :*: y_) = (hd y_, hd y_)

instance GSwizzle2 a => GSwizzle2 (M1 i c a) where
	type GX (M1 i c a) = GX a
	type GY (M1 i c a) = GY a
	gx (M1 a) = gx a
	gy (M1 a) = gy a
	gxx (M1 a) = gxx a
	gxy (M1 a) = gxy a
	gyx (M1 a) = gyx a
	gyy (M1 a) = gyy a

class One f where
	type OneType f
	one :: f a -> OneType f

instance One a => One (M1 i c a) where
	type OneType (M1 i c a) = OneType a
	one (M1 a) = one a

instance One (K1 i a) where
	type OneType (K1 i a) = a
	one (K1 a) = a

class Swizzle1 f where
	type HeadType f
	hd :: f a -> HeadType f

instance Swizzle1 a => Swizzle1 (M1 i c a) where
	type HeadType (M1 i c a) = HeadType a
	hd (M1 a) = hd a

instance Swizzle1 (K1 i a) where
	type HeadType (K1 i a) = a
	hd (K1 a) = a

instance One x => Swizzle1 (x :*: y) where
	type HeadType (x :*: y) = OneType x
	hd (x_ :*: _y) = one x_

instance Swizzle2 (x, y) where
	type X (x, y) = x
	type Y (x, y) = y

data Point = Point Double Double deriving (Show, Generic)

instance Swizzle2 Point where
	type X Point = Double
	type Y Point = Double

instance Swizzle2 (x, y, z) where
	type X (x, y, z) = x
	type Y (x, y, z) = y

data Foo = Foo (Double, Double) (Double, Double) deriving (Show, Generic)

instance Swizzle2 Foo where
	type X Foo = (Double, Double)
	type Y Foo = (Double, Double)
