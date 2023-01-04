{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.VertexInputState.Middle.Internal where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.Enum
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Data.Bits
import Data.Word

import qualified Gpu.Vulkan.VertexInput.Middle.Internal as VertexInput
import qualified Gpu.Vulkan.Pipeline.VertexInputState.Core as C

#include <vulkan/vulkan.h>

enum "CreateFlags" ''#{type VkPipelineVertexInputStateCreateFlags}
		[''Show, ''Storable, ''Eq, ''Bits] [("CreateFlagsZero", 0)]

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoVertexBindingDescriptions ::
		[VertexInput.BindingDescription],
	createInfoVertexAttributeDescriptions ::
		[VertexInput.AttributeDescription] }
	deriving Show

createInfoToCoreNew ::
	Pointable n => CreateInfo n -> ContT r IO (Ptr C.CreateInfo)
createInfoToCoreNew ci = do
	C.CreateInfo_ fCreateInfo <- createInfoToCore ci
	ContT $ withForeignPtr fCreateInfo

createInfoToCore :: Pointable n => CreateInfo n -> ContT r IO C.CreateInfo
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlags flgs,
	createInfoVertexBindingDescriptions =
		((length &&& id) . (VertexInput.bindingDescriptionToCore <$>))
			-> (vbdc, vbds),
	createInfoVertexAttributeDescriptions =
		((length &&& id) . (VertexInput.attributeDescriptionToCore <$>))
			-> (vadc, vads) } = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	pvbds <- ContT $ allocaArray vbdc
	lift $ pokeArray pvbds vbds
	pvads <- ContT $ allocaArray vadc
	lift $ pokeArray pvads vads
	pure C.CreateInfo {
		C.createInfoSType = (),
		C.createInfoPNext = pnxt,
		C.createInfoFlags = flgs,
		C.createInfoVertexBindingDescriptionCount = fromIntegral vbdc,
		C.createInfoPVertexBindingDescriptions = pvbds,
		C.createInfoVertexAttributeDescriptionCount = fromIntegral vadc,
		C.createInfoPVertexAttributeDescriptions = pvads }
