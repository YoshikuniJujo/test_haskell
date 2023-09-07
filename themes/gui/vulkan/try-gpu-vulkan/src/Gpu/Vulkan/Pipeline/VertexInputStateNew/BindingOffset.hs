{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.VertexInputStateNew.BindingOffset where

import GHC.Generics
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
