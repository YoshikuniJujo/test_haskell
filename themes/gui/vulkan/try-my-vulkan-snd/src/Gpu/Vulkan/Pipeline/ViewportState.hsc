{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.ViewportState where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.Enum
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Data.Default
import Data.Bits
import Data.Word

import Gpu.Vulkan.Core

import qualified Gpu.Vulkan.Pipeline.ViewportState.Core as C

#include <vulkan/vulkan.h>

enum "CreateFlags" ''#{type VkPipelineViewportStateCreateFlags}
	[''Show, ''Eq, ''Storable, ''Bits] [("CreateFlagsZero", 0)]

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoViewports :: [Viewport],
	createInfoScissors :: [Rect2d] }
	deriving Show

instance Default (CreateInfo n) where
	def = CreateInfo {
		createInfoNext = Nothing, createInfoFlags = zeroBits,
		createInfoViewports = [], createInfoScissors = [] }

createInfoToCore :: Pointable n => CreateInfo n -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlags flgs,
	createInfoViewports = (length &&& id) -> (vpc, vps),
	createInfoScissors = (length &&& id) -> (scc, scs) } = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	pvps <- ContT $ allocaArray vpc
	lift $ pokeArray pvps vps
	pscs <- ContT $ allocaArray scc
	lift $ pokeArray pscs scs
	let	C.CreateInfo_ fCreateInfo = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt,
			C.createInfoFlags = flgs,
			C.createInfoViewportCount = fromIntegral vpc,
			C.createInfoPViewports = pvps,
			C.createInfoScissorCount = fromIntegral scc,
			C.createInfoPScissors = pscs }
	ContT $ withForeignPtr fCreateInfo
