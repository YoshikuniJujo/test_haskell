{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.InputAssemblyState.Middle.Internal (
	CreateInfo(..), createInfoToCore
	) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.C.Enum
import Foreign.Pointable
import Control.Monad.Cont
import Data.Bits
import Data.Word

import Gpu.Vulkan.Base
import Gpu.Vulkan.Enum

import qualified Gpu.Vulkan.Pipeline.InputAssemblyState.Core as C

#include <vulkan/vulkan.h>

enum "CreateFlags" ''#{type VkPipelineInputAssemblyStateCreateFlags}
	[''Show, ''Eq, ''Storable, ''Bits] [("CreateFlagsZero", 0)]

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoTopology :: PrimitiveTopology,
	createInfoPrimitiveRestartEnable :: Bool }
	deriving Show

createInfoToCore :: Pointable n => CreateInfo n -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlags flgs,
	createInfoTopology = PrimitiveTopology tplg,
	createInfoPrimitiveRestartEnable = pre
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	let	C.CreateInfo_ fCreateInfo = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt,
			C.createInfoFlags = flgs,
			C.createInfoTopology = tplg,
			C.createInfoPrimitiveRestartEnable = boolToBool32 pre }
	ContT $ withForeignPtr fCreateInfo
