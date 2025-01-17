{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TrySwizzleSet where

import GHC.Generics

class GSwizzleSet1 f where
	type GX f

	gx :: GX f -> f a -> f a

instance GSwizzleSet1 (K1 i a) where
	type GX (K1 i a) = a
	gx a (K1 _) = K1 a
