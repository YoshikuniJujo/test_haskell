{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.VertexInputState.GetOffset where

import GHC.Generics
import Foreign.Storable

class FindOffset a t where
	findOffset :: Maybe Int

	default findOffset :: (Generic a, GFindOffset (Rep a) t) => Maybe Int
	findOffset = gFindOffset @(Rep a) @t

class GFound (f :: * -> *) t where gFound :: Bool
instance GFound (K1 i t) t where gFound = True
instance {-# OVERLAPPABLE #-} GFound (K1 i a) t where gFound = False
instance GFound a t => GFound (M1 i c a) t where gFound = gFound @a @t

class GSize (f :: * -> *) where gSize :: Int
instance Storable a => GSize (K1 i a) where gSize = sizeOf @a undefined
instance GSize a => GSize (M1 i c a) where gSize = gSize @a

class GFindOffset (f :: * -> *) t where gFindOffset :: Maybe Int
instance GFindOffset (K1 i t) t where gFindOffset = Just 0
instance {-# OVERLAPPABLE #-} GFindOffset (K1 i a) t where gFindOffset = Nothing

instance GFindOffset a t => GFindOffset (M1 i c a) t where
	gFindOffset = gFindOffset @a @t

instance (GFound a t, GSize a, GFindOffset b t) =>
	GFindOffset (M1 i c a :*: b) t where
	gFindOffset = if gFound @a @t then Just 0 else
		(gSize @a +) <$> gFindOffset @b @t

instance GFindOffset (a :*: b :*: c) t => GFindOffset ((a :*: b) :*: c) t where
	gFindOffset = gFindOffset @(a :*: b :*: c) @t

instance GFindOffset (Rep (a, b, c)) t => FindOffset (a, b, c) t
instance GFindOffset (Rep (a, b, c, d)) t => FindOffset (a, b, c, d) t

sampleFun :: forall a b c .
	(Storable a, FindOffset (a, b, c) b) => (a, b, c) -> b -> Maybe Int
sampleFun _ _ = findOffset @(a, b, c) @b
