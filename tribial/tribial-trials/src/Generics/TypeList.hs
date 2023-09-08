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

instance (Generic t0, Index' (Rep t0) ts t) => Index (t0 ': ts) t where
	index = index' @(Rep t0) @ts @t

class Index' (t0 :: Type -> Type) (ts :: [Type]) (t :: Type) where
	index' :: (Int, Int)

instance Index' body ts t => Index' (M1 _i _c body) ts t where
	index' = index' @body @ts @t

instance Index' (K1 _i t) ts t where index' = (0, 0)

instance {-# OVERLAPPABLE #-} Index ts t => Index' (K1 R t0) ts t where
	index' = (+ 1) `first` index @ts @t

instance Index' (body :*: bs) ts t => Index' (M1 _i _c body :*: bs) ts t where
	index' = index' @(body :*: bs) @ts @t

instance Index' (K1 R t :*: _ms) ts t where index' = (0, 0)

instance {-# OVERLAPPABLE #-} Index' ms ts t => Index' (_m :*: ms) ts t where
	index' = (+ 1) `second` index' @ms @ts @t
