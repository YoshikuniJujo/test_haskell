{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.VertexInputState.Middle where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Data.Word

import qualified Vulkan.VertexInput as VertexInput
import qualified Vulkan.Pipeline.VertexInputState.Core as C

#include <vulkan/vulkan.h>

newtype CreateFlags = CreateFlags #{type VkPipelineVertexInputStateCreateFlags}
	deriving Show

pattern CreateFlagsZero :: CreateFlags
pattern CreateFlagsZero <- CreateFlags 0 where
	CreateFlagsZero = CreateFlags 0

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoVertexBindingDescriptions ::
		[VertexInput.BindingDescription],
	createInfoVertexAttributeDescriptions ::
		[VertexInput.AttributeDescription] }
	deriving Show

createInfoToCore :: Pointable n => CreateInfo n -> ContT r IO C.CreateInfo
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlags flgs,
	createInfoVertexBindingDescriptions = length &&& id -> (vbdc, vbds),
	createInfoVertexAttributeDescriptions = length &&& id -> (vadc, vads)
	} = do
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
