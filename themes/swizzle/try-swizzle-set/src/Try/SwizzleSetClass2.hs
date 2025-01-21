{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try.SwizzleSetClass2 where

import GHC.Generics
import Data.Kind

class GSwizzleSet1 s b where
	type GX s b :: k -> Type
	gx :: s a -> b -> GX s b a

instance GSwizzleSet1 (K1 i a) b where
	type GX (K1 i a) b = K1 i b
	gx (K1 _) v = K1 v

instance GSwizzleSet1 a b => GSwizzleSet1 (M1 i c a) b where
	type GX (M1 i c a) b = M1 i c (GX a b)
	gx (M1 s) v = M1 $ gx s v

instance GSwizzleSet1 a b => GSwizzleSet1 (M1 i c a :*: g)  b where
	type GX (M1 i c a :*: g) b = M1 i c (GX a b) :*: g
	gx (s :*: t) v = gx s v :*: t

type family F s where
	F (a :*: (b :*: c)) = (a :*: b) :*: c

instance (
	GSwizzleSet1 (a :*: (b :*: c)) v,
	GX (a :*: (b :*: c)) v ~ GX a v :*: (b :*: c)
	) =>
	GSwizzleSet1 ((a :*: b) :*: c) v where
	type GX ((a :*: b) :*: c) v = F (GX (a :*: (b :*: c)) v)
	gx ((x :*: y) :*: z) v = let
		(x' :*: (y' :*: z')) = gx (x :*: (y :*: z)) v in
		((x' :*: y') :*: z')

class SwizzleSet1 s b where
	type X s b
	x :: s -> b -> X s b

	default x :: (
		Generic s, Generic (X s b),
		GSwizzleSet1 (Rep s) b,
		Rep (X s b) ~ GX (Rep s) b ) =>
		s -> b -> X s b
	x s b = to (gx (from s) b)

instance SwizzleSet1 (a, b, c) d where type X (a, b, c) d = (d, b, c)
