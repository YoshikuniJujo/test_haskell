{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.TessellationState.Middle.Internal where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.C.Enum
import Data.Bits
import Data.Word

import qualified Gpu.Vulkan.Pipeline.TessellationState.Core as C

#include <vulkan/vulkan.h>

enum "CreateFlags" ''#{type VkPipelineTessellationStateCreateFlags}
	[''Show, ''Eq, ''Bits, ''Storable] []

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoPatchControlPoints :: Word32 }
	deriving Show

createInfoToCore :: Pokable n =>
	CreateInfo n -> (Ptr C.CreateInfo -> IO a) -> IO a
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlags flgs,
	createInfoPatchControlPoints = pcps
	} f =
	withPokedMaybe mnxt \(castPtr -> pnxt) ->
	let ci = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt,
			C.createInfoFlags = flgs,
			C.createInfoPatchControlPoints = pcps } in
	withPoked ci f
