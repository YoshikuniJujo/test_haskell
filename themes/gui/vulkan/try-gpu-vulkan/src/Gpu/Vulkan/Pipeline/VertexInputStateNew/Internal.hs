{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.VertexInputStateNew.Internal (

	-- * CREATE INFO

	CreateInfo(..),

	-- * CREATE INFO TO MIDDLE

	createInfoToMiddle,
	BindingStrideList, AttributeDescriptions, Formattable(..)

	) where

import GHC.TypeNats
import Foreign.Storable.SizeAlignment
import Data.TypeLevel.TypeVal qualified as TypeVal
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.Tuple.MapIndex qualified as TMapIndex
import Data.Kind
import Data.Bits
import Data.Default

import qualified Gpu.Vulkan.Pipeline.VertexInputState.Middle as M
import qualified Gpu.Vulkan.VertexInput.Internal as VtxInp
import qualified Gpu.Vulkan.VertexInput.Middle as VtxInp.M

import Gpu.Vulkan.Pipeline.VertexInputStateNew.BindingOffset
import Gpu.Vulkan.Pipeline.VertexInputStateNew.CreateInfo
import Gpu.Vulkan.Pipeline.VertexInputStateNew.Formattable

-- Binding Descriptions

bindingDescriptions ::
	forall vibs . BindingStrideList vibs VtxInp.Rate =>
	[VtxInp.M.BindingDescription]
bindingDescriptions = fmap VtxInp.bindingDescriptionToMiddle
	. VtxInp.bindingDescriptionFromRaw
	$ bindingStrideList @_ @vibs @VtxInp.Rate

class BindingStrideList (ts :: [(Type, k)]) v where
	bindingStrideList :: [(SizeAlignment, v)]

instance BindingStrideList '[] v where bindingStrideList = []

instance (SizeAlignmentList t, BindingStrideList ts v, TypeVal.T a v) =>
	BindingStrideList ('(t, a) ': ts) v where
	bindingStrideList =
		(wholeSizeAlignment @t, TypeVal.t @_ @a @v) :
		bindingStrideList @_ @ts @v

-- Attribute Descriptions

class AttributeDescriptions
	(vibs :: [(Type, VtxInp.Rate)]) (vias :: [(Nat, Type)]) where
	attributeDescriptions :: [VtxInp.AttributeDescription]

instance AttributeDescriptions vibs '[] where attributeDescriptions = []

instance (
	KnownNat i, BindingOffsetNew (TMapIndex.M0_2 vibs) t, Formattable t,
	AttributeDescriptions vibs vias ) =>
	AttributeDescriptions vibs ('(i, t) ': vias) where
	attributeDescriptions = VtxInp.AttributeDescription {
		VtxInp.attributeDescriptionLocation =
			fromIntegral $ natVal @i undefined,
		VtxInp.attributeDescriptionBinding = bdng,
		VtxInp.attributeDescriptionFormat = formatOf @t,
		VtxInp.attributeDescriptionOffset = ost } : ads
		where
		(fromIntegral -> bdng, fromIntegral -> ost) =
			bindingOffsetNew @(TMapIndex.M0_2 vibs) @t
		ads = attributeDescriptions @vibs @vias
