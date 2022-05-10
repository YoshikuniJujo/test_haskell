{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.VertexInputState where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Pointable
import Control.Monad.Cont
import Data.Kind

import Vulkan.Enum
import Vulkan.Pipeline.VertexInputState.BindingStrideList
import Vulkan.Pipeline.VertexInputState.BindingOffset

import qualified Vulkan.Pipeline.VertexInputState.Middle as M
import qualified Vulkan.Pipeline.VertexInputState.Core as C
import qualified Vulkan.VertexInput as VertexInput

data CreateInfo n vs (ts :: [Type]) = CreateInfo {
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

class CreateInfoAttributeDescription vs (ts :: [Type]) where	
	createInfoAttributeDescriptions ::
		CreateInfo n vs ts -> [VertexInput.AttributeDescription]

instance CreateInfoAttributeDescription vs '[] where
	createInfoAttributeDescriptions _ = []

instance (
	BindingOffset vs t, Formattable t,
	CreateInfoAttributeDescription vs ts) =>
	CreateInfoAttributeDescription vs (t ': ts) where
	createInfoAttributeDescriptions :: forall n .
		CreateInfo n vs (t ': ts) -> [VertexInput.AttributeDescription]
	createInfoAttributeDescriptions _ = VertexInput.AttributeDescription {
		VertexInput.attributeDescriptionLocation = 0,
		VertexInput.attributeDescriptionBinding = bd,
		VertexInput.attributeDescriptionFormat = formatOf @t,
		VertexInput.attributeDescriptionOffset = os } :
		(VertexInput.succAttributeDescriptionLocation <$> ads)
		where
		Just (fromIntegral -> bd, fromIntegral -> os) = bindingOffset @vs @t
		ads = createInfoAttributeDescriptions @vs @ts undefined

class Formattable a where formatOf :: Format

instance Formattable Int where formatOf = FormatUndefined
