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

	createInfoToMiddle, AttributeDescriptions, Formattable(..),

	) where

import GHC.TypeNats
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.Kind
import Data.Bits
import Data.Default

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Pipeline.VertexInputState.BindingStrideList
import Gpu.Vulkan.Pipeline.VertexInputState.BindingOffset

import qualified Gpu.Vulkan.Pipeline.VertexInputState.Middle as M
import qualified Gpu.Vulkan.VertexInput as VtxInp
import qualified Gpu.Vulkan.VertexInput.Middle as VtxInp.M

-- CREATE INFO

data CreateInfo mn (vibs :: [(Type, VtxInp.Rate)]) (vias :: [(Nat, Type)]) =
	CreateInfo {
		createInfoNext :: TMaybe.M mn,
		createInfoFlags :: M.CreateFlags }

deriving instance Show (TMaybe.M mn) => Show (CreateInfo mn vs ts)

instance Default (CreateInfo 'Nothing vs ts) where
	def = CreateInfo {
		createInfoNext = TMaybe.N, createInfoFlags = zeroBits }

-- CREATE INFO TO MIDDLE

createInfoToMiddle :: forall n vibs vias . (
	BindingStrideList VtxInp.Rate vibs VtxInp.Rate,
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
	forall vs . BindingStrideList VtxInp.Rate vs VtxInp.Rate =>
	[VtxInp.M.BindingDescription]
bindingDescriptions = fmap VtxInp.bindingDescriptionToMiddle
	. VtxInp.bindingDescriptionFromRaw
	$ bindingStrideList @VtxInp.Rate @vs @VtxInp.Rate

-- Attribute Descriptions

class AttributeDescriptions
	(vs :: [(Type, VtxInp.Rate)]) (vias :: [(Nat, Type)]) where
	attributeDescriptions :: [VtxInp.AttributeDescription]

instance AttributeDescriptions vs '[] where
	attributeDescriptions = []

instance (
	BindingOffset vs t, Formattable t,
	AttributeDescriptions vs vias, KnownNat i) =>
	AttributeDescriptions vs ('(i, t) ': vias) where
	attributeDescriptions = VtxInp.AttributeDescription {
		VtxInp.attributeDescriptionLocation = fromIntegral $ natVal @i undefined,
		VtxInp.attributeDescriptionBinding = bd,
		VtxInp.attributeDescriptionFormat = formatOf @t,
		VtxInp.attributeDescriptionOffset = os } : ads
		where
		Just (fromIntegral -> bd, fromIntegral -> os) = bindingOffset @_ @vs @t
		ads = attributeDescriptions @vs @vias

class Formattable a where formatOf :: Format

instance Formattable Int where formatOf = FormatUndefined
