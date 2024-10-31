{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.MultisampleState.Middle.Internal (
	CreateInfo(..), CreateFlags, createInfoToCore ) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.C.Enum
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.Bits
import Data.Word

import Gpu.Vulkan.Base.Middle.Internal

import Gpu.Vulkan.Sample.Middle.Internal as Sample
import Gpu.Vulkan.Sample.Enum as Sample

import qualified Gpu.Vulkan.Pipeline.MultisampleState.Core as C

#include <vulkan/vulkan.h>

enum "CreateFlags" ''#{type VkPipelineMultisampleStateCreateFlags}
	[''Show, ''Eq, ''Storable, ''Bits] [("CreateFlagsZero", 0)]

data CreateInfo mn = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoRasterizationSamplesAndMask :: Sample.CountAndMask,
	createInfoSampleShadingEnable :: Bool,
	createInfoMinSampleShading :: Float,
	createInfoAlphaToCoverageEnable :: Bool,
	createInfoAlphaToOneEnable :: Bool }

deriving instance Show (TMaybe.M mn) => Show (CreateInfo mn)

createInfoToCore :: WithPoked (TMaybe.M mn) =>
	CreateInfo mn -> (Ptr C.CreateInfo -> IO a) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlags flgs,
	createInfoRasterizationSamplesAndMask = cm,
	createInfoSampleShadingEnable = boolToBool32 -> sse,
	createInfoMinSampleShading = mss,
	createInfoAlphaToCoverageEnable = boolToBool32 -> ace,
	createInfoAlphaToOneEnable = boolToBool32 -> aoe } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	countAndMaskToCore cm \(Sample.CountFlagBits c, m) ->
	let ci = C.CreateInfo {
		C.createInfoSType = (),
		C.createInfoPNext = pnxt',
		C.createInfoFlags = flgs,
		C.createInfoRasterizationSamples = c,
		C.createInfoSampleShadingEnable = sse,
		C.createInfoMinSampleShading = mss,
		C.createInfoPSampleMask = m,
		C.createInfoAlphaToCoverageEnable = ace,
		C.createInfoAlphaToOneEnable = aoe } in
	withPoked ci f
