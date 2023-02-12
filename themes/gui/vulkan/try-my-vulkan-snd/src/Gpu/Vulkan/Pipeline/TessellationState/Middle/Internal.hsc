{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.TessellationState.Middle.Internal where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.C.Enum
import Control.Monad.Cont
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

createInfoToCore :: Pokable n => CreateInfo n -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlags flgs,
	createInfoPatchControlPoints = pcps
	} = do
	(castPtr -> pnxt) <- ContT $ withPokedMaybe mnxt
	let C.CreateInfo_ fCreateInfo = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt,
			C.createInfoFlags = flgs,
			C.createInfoPatchControlPoints = pcps }
	ContT $ withForeignPtr fCreateInfo
