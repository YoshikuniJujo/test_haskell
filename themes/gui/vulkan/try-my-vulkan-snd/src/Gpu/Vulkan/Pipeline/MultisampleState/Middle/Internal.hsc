{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.MultisampleState.Middle.Internal (
	CreateInfo(..), createInfoToCore ) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.C.Enum
import Foreign.Pointable
import Control.Monad.Cont
import Data.Bits
import Data.Word

import Gpu.Vulkan.Misc

import Gpu.Vulkan.Sample.Middle.Internal as Sample
import Gpu.Vulkan.Sample.Enum as Sample

import qualified Gpu.Vulkan.Pipeline.MultisampleState.Core as C

#include <vulkan/vulkan.h>

enum "CreateFlags" ''#{type VkPipelineMultisampleStateCreateFlags}
	[''Show, ''Eq, ''Storable, ''Bits] [("CreateFlagsZero", 0)]

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoRasterizationSamplesAndMask :: Sample.CountAndMask,
	createInfoSampleShadingEnable :: Bool,
	createInfoMinSampleShading :: Float,
	createInfoAlphaToCoverageEnable :: Bool,
	createInfoAlphaToOneEnable :: Bool }
	deriving Show

createInfoToCore :: Pointable n => CreateInfo n -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlags flgs,
	createInfoRasterizationSamplesAndMask = cm,
	createInfoSampleShadingEnable = boolToBool32 -> sse,
	createInfoMinSampleShading = mss,
	createInfoAlphaToCoverageEnable = boolToBool32 -> ace,
	createInfoAlphaToOneEnable = boolToBool32 -> aoe
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	(Sample.CountFlagBits c, m) <- countAndMaskToCore cm
	let C.CreateInfo_ fCreateInfo = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt,
			C.createInfoFlags = flgs,
			C.createInfoRasterizationSamples = c,
			C.createInfoSampleShadingEnable = sse,
			C.createInfoMinSampleShading = mss,
			C.createInfoPSampleMask = m,
			C.createInfoAlphaToCoverageEnable = ace,
			C.createInfoAlphaToOneEnable = aoe }
	ContT $ withForeignPtr fCreateInfo
