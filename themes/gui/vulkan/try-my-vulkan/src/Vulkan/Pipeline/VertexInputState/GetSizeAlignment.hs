{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.VertexInputState.GetSizeAlignment where

import GHC.Generics
import Foreign.Storable

type Size = Int
type Alignment = Int

class FindSizeAlignment a t where
	findSizeAlignmentList :: Maybe [(Size, Alignment)]

class GFoundSizeAlignment (f :: * -> *) t where
	gFoundSizeAlignment :: Maybe (Size, Alignment)

instance Storable t => GFoundSizeAlignment (K1 i t) t where
	gFoundSizeAlignment = Just (sizeOf @t undefined, alignment @t undefined)

instance {-# OVERLAPPABLE #-} GFoundSizeAlignment (K1 i a) t where
	gFoundSizeAlignment = Nothing

instance GFoundSizeAlignment a t => GFoundSizeAlignment (M1 i c a) t where
	gFoundSizeAlignment = gFoundSizeAlignment @a @t

class GFindSizeAlignment (f :: * -> *) t where
	gFindSizeAlignmentList :: Maybe [(Size, Alignment)]

instance Storable t => GFindSizeAlignment (K1 i t) t where
	gFindSizeAlignmentList =
		Just [(sizeOf @t undefined, alignment @t undefined)]

instance (GFoundSizeAlignment a t, GFindSizeAlignment b t) =>
	GFindSizeAlignment (M1 i c a :*: b) t where
	gFindSizeAlignmentList = case gFoundSizeAlignment @a @t of
		Just sa -> Just [sa]
--		Nothing -> 
