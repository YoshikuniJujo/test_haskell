{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.VertexInputState.GetBind where

import GHC.Generics

import Vulkan.Pipeline.VertexInputState.GetOffset

class FindBind a t where
	findBind :: Maybe (Int, Int)

	default findBind :: (Generic a, GFindBind (Rep a) t) => Maybe (Int, Int)
	findBind = gFindBind @(Rep a) @t

class GFoundBind (f :: * -> *) t where gFoundBind :: Maybe Int

instance FindOffset a t => GFoundBind (K1 i [a]) t where
	gFoundBind = findOffset @a @t

instance GFoundBind a t => GFoundBind (M1 i c a) t where
	gFoundBind = gFoundBind @a @t

class GFindBind (f :: * -> *) t where gFindBind :: Maybe (Int, Int)

instance FindOffset a t => GFindBind (K1 i [a]) t where
	gFindBind = (0 ,) <$> findOffset @a @t

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

instance (FindOffset a t, FindOffset b t, FindOffset c t) =>
	FindBind ([a], [b], [c]) t
