{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.VertexInputState.GetBind where

import GHC.Generics
import Foreign.Storable.SizeAlignment

import Vulkan.Pipeline.VertexInputState.Flatten
import Vulkan.Pipeline.VertexInputState.BindingStrideList

{-
class FindBindAddType a t where
	findBindAddType :: Maybe (int, Int)

instance FindBind (MapSubType (Flatten (Rep a))) t => FindBindAddType a t where
	findBindAddType = findBind @(MapSubType (Flatten (Rep a))) @t
	-}

class FindBind a t where
	findBind :: Maybe (Int, Int)

	default findBind :: (Generic a, GFindBind (Rep a) t) => Maybe (Int, Int)
	findBind = gFindBind @(Rep a) @t

instance (Generic a, GFindBind (Rep a) t) => FindBind a t

class GFoundBind (f :: * -> *) t where gFoundBind :: Maybe Int

instance SizeAlignmentListUntil t a => GFoundBind (K1 i [a]) t where
	gFoundBind = offsetOf @t @a

instance GFoundBind a t => GFoundBind (M1 i c a) t where
	gFoundBind = gFoundBind @a @t

class GFindBind (f :: * -> *) t where gFindBind :: Maybe (Int, Int)

instance SizeAlignmentListUntil t a => GFindBind (K1 i [a]) t where
	gFindBind = (0 ,) <$> offsetOf @t @a

instance GFindBind a t => GFindBind (M1 i c a) t where
	gFindBind = gFindBind @a @t

instance (GFoundBind a t, GFindBind b t) =>
	GFindBind (M1 i c a :*: b) t where
	gFindBind = case gFoundBind @a @t of
		Just o -> Just (0, o)
		Nothing -> do
			(b, o) <- gFindBind @b @t
			pure (b + 1, o)

instance GFindBind (a :*: b :*: c) t => GFindBind ((a :*: b) :*: c) t where
	gFindBind = gFindBind @(a :*: b :*: c) @t

{-
instance (
	SizeAlignmentListUntil t a,
	SizeAlignmentListUntil t b, SizeAlignmentListUntil t c) =>
	FindBind ([a], [b], [c]) t
	-}
