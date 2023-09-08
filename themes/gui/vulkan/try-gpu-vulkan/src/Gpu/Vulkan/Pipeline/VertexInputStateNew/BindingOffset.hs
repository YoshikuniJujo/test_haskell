{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.VertexInputStateNew.BindingOffset (BindingOffsetNew(..)) where

import GHC.Generics
import Foreign.Storable
import Control.Arrow
import Data.Kind
import Gpu.Vulkan.Pipeline.VertexInputStateNew.GHC.Generics.TypeFam
import Gpu.Vulkan.Pipeline.VertexInputStateNew.Data.Type.TypeFam
import Gpu.Vulkan.Pipeline.VertexInputStateNew.Data.Type.TypeValMap

import Foreign.Storable.PeekPoke

class BindingOffset (tss :: [Type]) t where bindingOffset :: Maybe (Int, Offset)
instance BindingOffset '[] t where bindingOffset = Nothing

instance (SizeAlignmentListUntil t ts, BindingOffset tss t) =>
	BindingOffset (ts ': tss) t where
	bindingOffset = case offsetOf @t @ts of
		Nothing -> ((+ 1) `first`) <$> bindingOffset @tss @t
		Just os -> Just (0, os)

type Offset = Int

offsetOf :: forall t ts . SizeAlignmentListUntil t ts => Maybe Offset
offsetOf = calcOffset <$> sizeAlignmentListUntil @t @ts

calcOffset :: [SizeAlignment] -> Offset
calcOffset = foldl next 0 . shiftAlignmentL

next :: Offset -> SizeAlignment -> Offset
next os (sz, algn) = ((os + sz - 1) `div` algn + 1) * algn

shiftAlignmentL :: [SizeAlignment] -> [SizeAlignment]
shiftAlignmentL [] = error "empty size and alignment list"
shiftAlignmentL sas = zip ss as where (ss, _ : as) = unzip sas

class SizeAlignmentListUntil t a where
	sizeAlignmentListUntil :: Maybe [SizeAlignment]

	default sizeAlignmentListUntil :: (
		MapTypeValMaybe2 Sizable (Until t (Flatten (Rep a))) ) =>
		Maybe [SizeAlignment]
	sizeAlignmentListUntil =
		sizeAlignmentTypeMaybeList @(Until t (Flatten (Rep a)))

sizeAlignmentTypeMaybeList ::
	forall (mas :: Maybe [Type]) . MapTypeValMaybe2 Sizable mas =>
	Maybe [SizeAlignment]
sizeAlignmentTypeMaybeList =
	mapTypeValMaybe2 @Sizable @mas (\(_ :: a) -> (sizeOf' @a, alignment' @a))

type Size = Int
type Alignment = Int
type SizeAlignment = (Size, Alignment)

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
