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

class Index (ts :: [Type]) (a :: Type) where index :: (Int, Int)

instance (Generic t0, Index' (Rep t0) ts a) => Index (t0 ': ts) a where
	index = index' @(Rep t0) @ts @a 0

class Index' (t0 :: Type -> Type) (ts :: [Type]) (a :: Type) where
	index' :: Int -> (Int, Int)

instance Index' body ts a => Index' (M1 _i _c body) ts a where
	index' = index' @body @ts @a

instance Index' (K1 _i a) ts a where index' i = (0, i)

instance {-# OVERLAPPABLE #-} Index ts a => Index' (K1 _i t0) ts a where
	index' _ = (+ 1) `first` index @ts @a

instance Index' (body :*: bs) ts a => Index' (M1 _i _c body :*: bs) ts a where
	index' = index' @(body :*: bs) @ts @a

instance Index' (K1 _i a :*: _ms) ts a where index' i = (0, i)

instance Index' (m :*: (m' :*: ms)) ts a => Index' ((m :*: m') :*: ms) ts a where
	index' = index' @(m :*: (m' :*: ms)) @ts @a

instance {-# OVERLAPPABLE #-} Index' ms ts a => Index' (_m :*: ms) ts a where
	index' i = index' @ms @ts @a (i + 1)
