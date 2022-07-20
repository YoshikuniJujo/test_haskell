{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.VertexInputState where

import GHC.TypeNats
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Pointable
import Control.Monad.Cont
import Data.Kind

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Pipeline.VertexInputState.BindingStrideList
import Gpu.Vulkan.Pipeline.VertexInputState.BindingOffset

import qualified Gpu.Vulkan.Pipeline.VertexInputState.Middle as M
import qualified Gpu.Vulkan.Pipeline.VertexInputState.Core as C
import qualified Gpu.Vulkan.VertexInput as VertexInput

data CreateInfo n (vs :: [Type]) (ts :: [(Nat, Type)]) = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: M.CreateFlags }
	deriving Show

createInfoToCore :: (
	Pointable n,
	BindingStrideList vs VertexInput.Rate VertexInput.Rate,
	CreateInfoAttributeDescription vs ts ) =>
	CreateInfo n vs ts -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore ci = do
	C.CreateInfo_ fCreateInfo <- M.createInfoToCore $ createInfoToMiddle ci
	ContT $ withForeignPtr fCreateInfo

createInfoToMiddle :: (
	BindingStrideList vs VertexInput.Rate VertexInput.Rate,
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
	BindingStrideList vs
		VertexInput.Rate VertexInput.Rate =>
	CreateInfo n vs ts -> [VertexInput.BindingDescription]
createInfoToBindingDescriptions _ = VertexInput.bindingDescriptionFromRaw
	$ bindingStrideList @vs @VertexInput.Rate @VertexInput.Rate

class CreateInfoAttributeDescription (vs :: [Type]) (ts :: [(Nat, Type)]) where	
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
		Just (fromIntegral -> bd, fromIntegral -> os) = bindingOffset @vs @t
		ads = createInfoAttributeDescriptions @vs @ts undefined

class Formattable a where formatOf :: Format

instance Formattable Int where formatOf = FormatUndefined
