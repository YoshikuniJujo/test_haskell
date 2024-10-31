{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.VertexInputState.Middle.Internal where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.C.Enum
import Control.Arrow
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.Bits
import Data.Word

import qualified Gpu.Vulkan.VertexInput.Middle.Internal as VertexInput
import qualified Gpu.Vulkan.Pipeline.VertexInputState.Core as C

#include <vulkan/vulkan.h>

enum "CreateFlags" ''#{type VkPipelineVertexInputStateCreateFlags}
		[''Show, ''Storable, ''Eq, ''Bits] []

data CreateInfo mn = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoVertexBindingDescriptions ::
		[VertexInput.BindingDescription],
	createInfoVertexAttributeDescriptions ::
		[VertexInput.AttributeDescription] }

deriving instance Show (TMaybe.M mn) => Show (CreateInfo mn)

createInfoToCore :: WithPoked (TMaybe.M mn) =>
	CreateInfo mn -> (Ptr C.CreateInfo -> IO a) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlags flgs,
	createInfoVertexBindingDescriptions =
		((length &&& id) . (VertexInput.bindingDescriptionToCore <$>))
			-> (vbdc, vbds),
	createInfoVertexAttributeDescriptions =
		((length &&& id) . (VertexInput.attributeDescriptionToCore <$>))
			-> (vadc, vads) } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
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
