{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
-- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures, TypeOperators #-} -- , DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes, DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import GHC.Generics

class FindIndex a t where
	findIndex :: Int

	default findIndex :: (Generic a, GFindIndex (Rep a) t) => Int
	findIndex = gFindIndex @(Rep a) @t

class GFound (f :: * -> *) t where gFound :: Bool
instance GFound (K1 i t) t where gFound = True
instance {-# OVERLAPPABLE #-} GFound (K1 i a) t where gFound = False
instance GFound a t => GFound (M1 i c a) t where gFound = gFound @a @t

class GFindIndex (f :: * -> *) t where gFindIndex :: Int

instance GFindIndex (K1 i t) t where gFindIndex = 0
instance {-# OVERLAPPABLE #-} GFindIndex (K1 i a) t where gFindIndex = minBound

instance GFindIndex a t => GFindIndex (M1 i c a) t where
	gFindIndex = gFindIndex @a @t

instance (GFound a t, GFindIndex b t) => GFindIndex (M1 i c a :*: b) t where
	gFindIndex = if gFound @a @t then 0 else gFindIndex @b @t + 1

instance GFindIndex (a :*: b :*: c) t => GFindIndex ((a :*: b) :*: c) t where
	gFindIndex = gFindIndex @(a :*: b :*: c) @t

instance FindIndex (a, b) t where
	findIndex = gFindIndex @(Rep (a, b)) @t

instance FindIndex (t, b, c) t
instance FindIndex (a, t, c) t
instance FindIndex (a, b, t) t

instance FindIndex (Double, Int, (), Bool) Int

