{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.VertexInputState.BindingOffset (BindingOffsetNew(..)) where

import GHC.Generics
import Foreign.Storable
import Control.Arrow
import Data.Kind

type Offset = Int

class BindingOffsetNew (ts :: [Type]) (a :: Type) where
	bindingOffsetNew :: (Int, Offset)

instance BindingOffsetNew' (Rep t) ts a => BindingOffsetNew (t ': ts) a where
	bindingOffsetNew = bindingOffsetNew' @(Rep t) @ts @a 0

class BindingOffsetNew' (t :: Type -> Type) (ts :: [Type]) (a :: Type) where
	bindingOffsetNew' :: Offset -> (Int, Offset)

instance BindingOffsetNew' body ts a =>
	BindingOffsetNew' (M1 _i _c body) ts a where
	bindingOffsetNew' = bindingOffsetNew' @body @ts @a

instance Storable a => BindingOffsetNew' (K1 _i a) ts a where
	bindingOffsetNew' = (0 ,) . align @a

instance {-# OVERLAPPABLE #-} BindingOffsetNew ts a =>
	BindingOffsetNew' (K1 _i t) ts a where
	bindingOffsetNew' _ = (+ 1) `first` bindingOffsetNew @ts @a

instance BindingOffsetNew' (body :*: bs) ts a =>
	BindingOffsetNew' (M1 _i _c body :*: bs) ts a where
	bindingOffsetNew' = bindingOffsetNew' @(body :*: bs) @ts @a

instance Storable a => BindingOffsetNew' (K1 _i a :*: _bs) ts a where
	bindingOffsetNew' = (0 ,) . align @a

instance BindingOffsetNew' (b :*: (b' :*: bs)) ts a =>
	BindingOffsetNew' ((b :*: b') :*: bs) ts a where
	bindingOffsetNew' = bindingOffsetNew' @(b :*: (b' :*: bs)) @ts @a

instance {-# OVERLAPPABLE #-} (Storable t, BindingOffsetNew' bs ts a) =>
	BindingOffsetNew' (K1 _i t :*: bs) ts a where
	bindingOffsetNew' ofst = bindingOffsetNew' @bs @ts @a (nextEnd @t ofst)

nextEnd :: forall t . Storable t => Offset -> Offset
nextEnd ofst = align @t ofst + sizeOf @t undefined

align :: forall t . Storable t => Offset -> Offset
align ofst = ((ofst - 1) `div` algn + 1) * algn
	where algn = alignment @t undefined
