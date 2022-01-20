{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.VertexInputState.GetSizeAlignment where

import GHC.Generics
import Foreign.Storable
import Control.Arrow

type Size = Int
type Alignment = Int

-- FIND SIZE ALIGNMENT

class FindSizeAlignment a t where
	findSizeAlignmentList :: Maybe [(Size, Alignment)]

	default findSizeAlignmentList ::
		(Generic a, GFindSizeAlignment (Rep a) t) =>
		Maybe [(Size, Alignment)]
	findSizeAlignmentList = gFindSizeAlignmentList @(Rep a) @t

-- GENERIC FOUND SIZE ALIGNMENT

class GFoundSizeAlignment (f :: * -> *) t where
	gFoundSizeAlignment :: Maybe (Size, Alignment)

instance Storable t => GFoundSizeAlignment (K1 i t) t where
	gFoundSizeAlignment = Just (sizeOf @t undefined, alignment @t undefined)

instance {-# OVERLAPPABLE #-} GFoundSizeAlignment (K1 i a) t where
	gFoundSizeAlignment = Nothing

instance GFoundSizeAlignment a t => GFoundSizeAlignment (M1 i c a) t where
	gFoundSizeAlignment = gFoundSizeAlignment @a @t

-- GENERIC SIZE ALIGNMENT

class GSizeAlignment (f :: * -> *) where gSizeAlignment :: (Size, Alignment)
instance Storable a => GSizeAlignment (K1 i a) where
	gSizeAlignment = (sizeOf @a undefined, alignment @a undefined)
instance GSizeAlignment a => GSizeAlignment (M1 i c a) where
	gSizeAlignment = gSizeAlignment @a

-- GENERIC FIND SIZE ALIGNMENT

class GFindSizeAlignment (f :: * -> *) t where
	gFindSizeAlignmentList :: Maybe [(Size, Alignment)]

instance Storable t => GFindSizeAlignment (K1 i t) t where
	gFindSizeAlignmentList =
		Just [(sizeOf @t undefined, alignment @t undefined)]

instance {-# OVERLAPPABLE #-} GFindSizeAlignment (K1 i a) t where
	gFindSizeAlignmentList = Nothing

instance GFindSizeAlignment a t => GFindSizeAlignment (M1 i c a) t where
	gFindSizeAlignmentList = gFindSizeAlignmentList @a @t

instance (GFoundSizeAlignment a t, GSizeAlignment a, GFindSizeAlignment b t) =>
	GFindSizeAlignment (M1 i c a :*: b) t where
	gFindSizeAlignmentList = case gFoundSizeAlignment @a @t of
		Just sa -> Just [sa]
		Nothing ->
			(gSizeAlignment @a :) <$> gFindSizeAlignmentList @b @t

instance GFindSizeAlignment (a :*: b :*: c) t =>
	GFindSizeAlignment ((a :*: b) :*: c) t where
	gFindSizeAlignmentList = gFindSizeAlignmentList @(a :*: b :*: c) @t

instance GFindSizeAlignment (Rep (a, b, c, d)) t =>
	FindSizeAlignment (a, b, c, d) t

instance GFindSizeAlignment (Rep (a, b, c, d, e, f)) t =>
	FindSizeAlignment (a, b, c, d, e, f) t

-- GET OFFSET

foldl2 :: (s -> a -> b -> s) -> s -> [a] -> [b] -> s
foldl2 f s = (foldl (uncurry . f) s .) . zip

scanl2 :: (s -> a -> b -> s) -> s -> [a] -> [b] -> [s]
scanl2 f s = (scanl (uncurry . f) s .) . zip

type Offset = Int

nextOffset :: Offset -> Size -> Alignment -> Offset
nextOffset os sz algn = ((os + sz - 1) `div` algn + 1) * algn

offset :: [(Size, Alignment)] -> Offset
offset = uncurry (foldl2 nextOffset 0) . (tail `second`) . unzip

findOffset :: forall a t . FindSizeAlignment a t => Maybe Offset
findOffset = offset <$> (findSizeAlignmentList @a @t)
