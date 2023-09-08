{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Generics.TypeList where

import GHC.Generics
import Control.Arrow
import Data.Kind

data Foo = Foo Int Double Char deriving (Show, Generic)

class Index (ts :: [Type]) (t :: Type) where index :: (Int, Int)

class Index' (t0 :: Type -> Type) (ts :: [Type]) (t :: Type) where
	index' :: (Int, Int)

instance (Generic t0, Index' (Rep t0) ts t) => Index (t0 ': ts) t where
	index = index' @(Rep t0) @ts @t

instance Index' (M1 S _m (K1 R t)) ts t where index' = (0, 0)

instance {-# OVERLAPPABLE #-} Index ts t =>
	Index' (M1 S _m (K1 R t0)) ts t where
	index' = (+ 1) `first` index @ts @t

instance Index' (M1 S _m (K1 R t) :*: _ms) ts t where index' = (0, 0)

instance {-# OVERLAPPABLE #-} Index' ms ts t => Index' (_m :*: ms) ts t where
	index' = (+ 1) `second` index' @ms @ts @t

instance Index' body ts t => Index' (D1 _d (C1 _c body)) ts t where
	index' = index' @body @ts @t
