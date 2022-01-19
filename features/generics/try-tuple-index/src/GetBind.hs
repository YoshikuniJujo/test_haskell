{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GetBind where

import GHC.Generics
import Data.Maybe

import GetOffset

class FindBind a t where
	findBind :: Maybe Int

	default findBind :: (Generic a, GFindBind (Rep a) t) => Maybe Int
	findBind = gFindBind @(Rep a) @t

class GFoundBind (f :: * -> *) t where gFoundBind :: Bool

instance FindOffset a t => GFoundBind (K1 i [a]) t where
	gFoundBind = isJust $ findOffset @a @t

instance GFoundBind a t => GFoundBind (M1 i c a) t where
	gFoundBind = gFoundBind @a @t

class GFindBind (f :: * -> *) t where gFindBind :: Maybe Int

instance FindOffset a t => GFindBind (K1 i [a]) t where
	gFindBind = 0 <$ findOffset @a @t

instance GFindBind a t => GFindBind (M1 i c a) t where
	gFindBind = gFindBind @a @t

instance (GFoundBind a t, GFindBind b t) =>
	GFindBind (M1 i c a :*: b) t where
	gFindBind = if gFoundBind @a @t then Just 0 else
		(1 +) <$> gFindBind @b @t

instance GFindBind (a :*: b :*: c) t => GFindBind ((a :*: b) :*: c) t where
	gFindBind = gFindBind @(a :*: b :*: c) @t

instance (FindOffset a t, FindOffset b t, FindOffset c t) => FindBind ([a], [b], [c]) t
