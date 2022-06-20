{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.DynamicState where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.Enum
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Data.Word

import Vulkan.Enum

import qualified Vulkan.Pipeline.DynamicState.Core as C

#include <vulkan/vulkan.h>

enum "CreateFlags" ''#{type VkPipelineDynamicStateCreateFlags}
	[''Show, ''Storable] [("CreateFlagsZero", 0)]

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoDynamicStates :: [DynamicState] }
	deriving Show

createInfoToCore :: Pointable n => CreateInfo n -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlags flgs,
	createInfoDynamicStates = (length &&& ((\(DynamicState ds) -> ds) <$>)) -> (dsc, dss)
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	pdss <- ContT $ allocaArray dsc
	lift $ pokeArray pdss dss
	let	C.CreateInfo_ fCreateInfo = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt,
			C.createInfoFlags = flgs,
			C.createInfoDynamicStateCount = fromIntegral dsc,
			C.createInfoPDynamicStates = pdss }
	ContT $ withForeignPtr fCreateInfo
