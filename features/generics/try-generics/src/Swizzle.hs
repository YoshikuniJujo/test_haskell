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
	default x :: (Generic a, GSwizzle1 (Rep a), X a ~ GX (Rep a)) => a -> X a
	x = gx . from

xx :: Swizzle1 a => a -> (X a, X a)
xx a = (x a, x a)

class GSwizzle1 f where type GX f; gx :: f a -> GX f
instance GSwizzle1 (K1 i a) where type GX (K1 i a) = a; gx (K1 a) = a

instance GSwizzle1 a => GSwizzle1 (M1 i c a) where
	type GX (M1 i c a) = GX a; gx (M1 a) = gx a

instance GSwizzle1 a => GSwizzle1 (a :*: _b) where
	type GX (a :*: _b) = GX a; gx (x_ :*: _) = gx x_

class Swizzle1 a => Swizzle2 a where
	type Y a

	y :: a -> Y a
	default y ::
		(Generic a, GSwizzle2 (Rep a), Y a ~ GY (Rep a)) => a -> Y a
	y = gy . from

xy :: Swizzle2 a => a -> (X a, Y a)
xy a = (x a, y a)
yx :: Swizzle2 a => a -> (Y a, X a)
yx a = (y a, x a)
yy :: Swizzle2 a => a -> (Y a, Y a)
yy a = (y a, y a)

class GSwizzle1 f => GSwizzle2 f where type GY f; gy :: f a -> GY f

instance GSwizzle2 a => GSwizzle2 (M1 i c a) where
	type GY (M1 i c a) = GY a; gy (M1 a) = gy a

instance (GSwizzle1 a, GSwizzle1 b) => GSwizzle2 (a :*: b) where
	type GY (a :*: b) = GX b; gy (_a :*: b) = gx b

class Swizzle2 a => Swizzle3 a where
	type Z a

	z :: a -> Z a
	default z ::
		(Generic a, GSwizzle3 (Rep a), Z a ~ GZ (Rep a)) => a -> Z a
	z = gz . from

class GSwizzle2 f => GSwizzle3 f where type GZ f; gz :: f a -> GZ f

instance GSwizzle3 a => GSwizzle3 (M1 i c a) where
	type GZ (M1 i c a) = GZ a; gz (M1 a) = gz a

instance (GSwizzle1 a, GSwizzle2 b) => GSwizzle3 (a :*: b) where
	type GZ (a :*: b) = GY b; gz (_a :*: b) = gy b

instance Swizzle1 (x, y) where type X (x, y) = x
instance Swizzle2 (x, y) where type Y (x, y) = y

instance Swizzle1 (x, y, z) where type X (x, y, z) = x
instance Swizzle2 (x, y, z) where type Y (x, y, z) = y
instance Swizzle3 (x, y, z) where type Z (x, y, z) = z
