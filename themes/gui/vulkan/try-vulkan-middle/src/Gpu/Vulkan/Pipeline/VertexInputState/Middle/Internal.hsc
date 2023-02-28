{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.VertexInputState.Middle.Internal where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.C.Enum
import Control.Arrow
import Data.Bits
import Data.Word

import qualified Gpu.Vulkan.VertexInput.Middle.Internal as VertexInput
import qualified Gpu.Vulkan.Pipeline.VertexInputState.Core as C

#include <vulkan/vulkan.h>

enum "CreateFlags" ''#{type VkPipelineVertexInputStateCreateFlags}
		[''Show, ''Storable, ''Eq, ''Bits] []

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoVertexBindingDescriptions ::
		[VertexInput.BindingDescription],
	createInfoVertexAttributeDescriptions ::
		[VertexInput.AttributeDescription] }
	deriving Show

createInfoToCore :: WithPoked n =>
	CreateInfo n -> (Ptr C.CreateInfo -> IO a) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlags flgs,
	createInfoVertexBindingDescriptions =
		((length &&& id) . (VertexInput.bindingDescriptionToCore <$>))
			-> (vbdc, vbds),
	createInfoVertexAttributeDescriptions =
		((length &&& id) . (VertexInput.attributeDescriptionToCore <$>))
			-> (vadc, vads) } f =
	withPokedMaybe' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	allocaArray vbdc \pvbds ->
	pokeArray pvbds vbds >>
	allocaArray vadc \pvads ->
	pokeArray pvads vads >>
	let ci = C.CreateInfo {
		C.createInfoSType = (),
		C.createInfoPNext = pnxt',
		C.createInfoFlags = flgs,
		C.createInfoVertexBindingDescriptionCount = fromIntegral vbdc,
		C.createInfoPVertexBindingDescriptions = pvbds,
		C.createInfoVertexAttributeDescriptionCount = fromIntegral vadc,
		C.createInfoPVertexAttributeDescriptions = pvads } in
	withPoked ci f
