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

module Gpu.Vulkan.Pipeline.VertexInputState.Internal (

	-- * CREATE INFO

	CreateInfo(..),

	-- * CREATE INFO TO MIDDLE

	createInfoToMiddle,
	BindingStrideList, AttributeDescriptions, Formattable(..)

	) where

import GHC.TypeNats
import GHC.Generics
import Foreign.Storable.PeekPoke
import Data.TypeLevel.TypeVal qualified as TypeVal
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.Tuple.MapIndex qualified as TMapIndex
import Data.Kind
import Data.Bits
import Data.Default

import qualified Gpu.Vulkan.Pipeline.VertexInputState.Middle as M
import qualified Gpu.Vulkan.VertexInput.Internal as VtxInp
import qualified Gpu.Vulkan.VertexInput.Middle as VtxInp.M

import Gpu.Vulkan.Pipeline.VertexInputState.BindingOffset
import Gpu.Vulkan.Pipeline.VertexInputState.Formattable
import Gpu.Vulkan.Pipeline.VertexInputState.SizeAlignment
import Gpu.Vulkan.Pipeline.VertexInputState.Data.Type.TypeValMap
import Gpu.Vulkan.Pipeline.VertexInputState.GHC.Generics.TypeFam

-- CREATE INFO

data CreateInfo mn (vibs :: [(Type, VtxInp.Rate)]) (vias :: [(Nat, Type)]) =
	CreateInfo {
		createInfoNext :: TMaybe.M mn,
		createInfoFlags :: M.CreateFlags }

deriving instance Show (TMaybe.M mn) => Show (CreateInfo mn vibs vias)

instance Default (CreateInfo 'Nothing vibs vias) where
	def = CreateInfo {
		createInfoNext = TMaybe.N, createInfoFlags = zeroBits }

-- CREATE INFO TO MIDDLE

createInfoToMiddle :: forall n vibs vias . (
	BindingStrideList vibs VtxInp.Rate,
	AttributeDescriptions vibs vias ) =>
	CreateInfo n vibs vias -> M.CreateInfo n
createInfoToMiddle CreateInfo {
	createInfoNext = mnxt, createInfoFlags = flgs } = M.CreateInfo {
	M.createInfoNext = mnxt,
	M.createInfoFlags = flgs,
	M.createInfoVertexBindingDescriptions = bindingDescriptions @vibs,
	M.createInfoVertexAttributeDescriptions =
		attributeDescriptions @vibs @vias }

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

instance (MapTypeVal2 Sizable (Flatten (Rep t)), BindingStrideList ts v, TypeVal.T a v) =>
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
