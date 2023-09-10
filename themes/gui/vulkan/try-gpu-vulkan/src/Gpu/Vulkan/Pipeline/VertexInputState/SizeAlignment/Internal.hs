{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE KindSignatures, DataKinds, ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.VertexInputState.SizeAlignment.Internal (
	SizeAlignmentList(sizeAlignmentList),
	SizeAlignmentListUntil(sizeAlignmentListUntil), MapSizableUntil,
	Size, Alignment, SizeAlignment ) where

import GHC.Generics
import Data.Kind
import Gpu.Vulkan.Pipeline.VertexInputState.GHC.Generics.TypeFam
import Gpu.Vulkan.Pipeline.VertexInputState.Data.Type.TypeFam
import Gpu.Vulkan.Pipeline.VertexInputState.Data.Type.TypeValMap

import Foreign.Storable.PeekPoke

type Size = Int
type Alignment = Int
type SizeAlignment = (Size, Alignment)

sizeAlignmentTypeList ::
	forall (as :: [Type]) . MapTypeVal2 Sizable as => [SizeAlignment]
sizeAlignmentTypeList = mapTypeVal2 @Sizable @as (\(_ :: a) -> (sizeOf' @a, alignment' @a))

class SizeAlignmentList a where
	sizeAlignmentList :: [SizeAlignment]

	default sizeAlignmentList :: (
		MapTypeVal2 Sizable (Flatten (Rep a)) ) => [SizeAlignment]
	sizeAlignmentList = sizeAlignmentTypeList @(Flatten (Rep a))

sizeAlignmentTypeMaybeList ::
	forall (mas :: Maybe [Type]) . MapTypeValMaybe2 Sizable mas =>
	Maybe [SizeAlignment]
sizeAlignmentTypeMaybeList =
	mapTypeValMaybe2 @Sizable @mas (\(_ :: a) -> (sizeOf' @a, alignment' @a))

class SizeAlignmentListUntil t a where
	sizeAlignmentListUntil :: Maybe [SizeAlignment]

	default sizeAlignmentListUntil :: (
		MapTypeValMaybe2 Sizable (Until t (Flatten (Rep a))) ) =>
		Maybe [SizeAlignment]
	sizeAlignmentListUntil =
		sizeAlignmentTypeMaybeList @(Until t (Flatten (Rep a)))

type MapSizableUntil t ts = MapTypeValMaybe2 Sizable (Until t (Flatten (Rep ts)))
