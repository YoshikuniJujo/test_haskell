{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.VertexInputState where

import GHC.TypeNats
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.Kind
import Data.Bits
import Data.Default

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Pipeline.VertexInputState.BindingStrideList
import Gpu.Vulkan.Pipeline.VertexInputState.BindingOffset

import qualified Gpu.Vulkan.Pipeline.VertexInputState.Middle as M
import qualified Gpu.Vulkan.VertexInput as VertexInput
import qualified Gpu.Vulkan.VertexInput.Middle as VertexInput.M

data CreateInfo mn (vs :: [(Type, VertexInput.Rate)]) (ts :: [(Nat, Type)]) = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: M.CreateFlags }

deriving instance Show (TMaybe.M mn) => Show (CreateInfo mn vs ts)

instance Default (CreateInfo 'Nothing vs ts) where
	def = CreateInfo {
		createInfoNext = TMaybe.N, createInfoFlags = zeroBits }

createInfoToMiddle :: (
	BindingStrideList VertexInput.Rate vs VertexInput.Rate,
	CreateInfoAttributeDescription vs ts ) =>
	CreateInfo n vs ts -> M.CreateInfo n
createInfoToMiddle
	ci@CreateInfo { createInfoNext = mnxt, createInfoFlags = flgs } =
	M.CreateInfo {
		M.createInfoNext = mnxt,
		M.createInfoFlags = flgs,
		M.createInfoVertexBindingDescriptions =
			createInfoToBindingDescriptions ci,
		M.createInfoVertexAttributeDescriptions =
			createInfoAttributeDescriptions ci }

createInfoToBindingDescriptions :: forall n vs ts .
	BindingStrideList 
		VertexInput.Rate vs VertexInput.Rate =>
	CreateInfo n vs ts -> [VertexInput.M.BindingDescription]
createInfoToBindingDescriptions _ = fmap VertexInput.bindingDescriptionToMiddle . VertexInput.bindingDescriptionFromRaw
	$ bindingStrideList @VertexInput.Rate @vs @VertexInput.Rate

class CreateInfoAttributeDescription (vs :: [(Type, VertexInput.Rate)]) (ts :: [(Nat, Type)]) where	
	createInfoAttributeDescriptions ::
		CreateInfo n vs ts -> [VertexInput.AttributeDescription]

instance CreateInfoAttributeDescription vs '[] where
	createInfoAttributeDescriptions _ = []

instance (
	BindingOffset vs t, Formattable t,
	CreateInfoAttributeDescription vs ts, KnownNat i) =>
	CreateInfoAttributeDescription vs ('(i, t) ': ts) where
	createInfoAttributeDescriptions :: forall n .
		CreateInfo n vs ('(i, t) ': ts) -> [VertexInput.AttributeDescription]
	createInfoAttributeDescriptions _ = VertexInput.AttributeDescription {
		VertexInput.attributeDescriptionLocation = fromIntegral $ natVal @i undefined,
		VertexInput.attributeDescriptionBinding = bd,
		VertexInput.attributeDescriptionFormat = formatOf @t,
		VertexInput.attributeDescriptionOffset = os } : ads
		where
		Just (fromIntegral -> bd, fromIntegral -> os) = bindingOffset @_ @vs @t
		ads = createInfoAttributeDescriptions @vs @ts undefined

class Formattable a where formatOf :: Format

instance Formattable Int where formatOf = FormatUndefined
