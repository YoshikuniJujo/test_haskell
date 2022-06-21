{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.ColorBlendState where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.Enum
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Data.Word
import Data.Color

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Base

import qualified Gpu.Vulkan.Pipeline.ColorBlendAttachment as ColorBlendAttachment
import qualified Gpu.Vulkan.Pipeline.ColorBlendState.Core as C

#include <vulkan/vulkan.h>

enum "CreateFlags" ''#{type VkPipelineColorBlendStateCreateFlags}
	[''Show, ''Storable] [("CreateFlagsZero", 0)]

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoLogicOpEnable :: Bool,
	createInfoLogicOp :: LogicOp,
	createInfoAttachments :: [ColorBlendAttachment.State],
	createInfoBlendConstants :: Rgba Float }
	deriving Show

createInfoToCore :: Pointable n => CreateInfo n -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlags flgs,
	createInfoLogicOpEnable = boolToBool32 -> loe,
	createInfoLogicOp = LogicOp lo,
	createInfoAttachments =
		length &&& (ColorBlendAttachment.stateToCore <$>) -> (ac, as),
	createInfoBlendConstants = RgbaDouble r g b a
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	pas <- ContT $ allocaArray ac
	lift $ pokeArray pas as
	let C.CreateInfo_ fCreateInfo = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt,
			C.createInfoFlags = flgs,
			C.createInfoLogicOpEnable = loe,
			C.createInfoLogicOp = lo,
			C.createInfoAttachmentCount = fromIntegral ac,
			C.createInfoPAttachments = pas,
			C.createInfoBlendConstants = [r, g, b, a] }
	ContT $ withForeignPtr fCreateInfo
