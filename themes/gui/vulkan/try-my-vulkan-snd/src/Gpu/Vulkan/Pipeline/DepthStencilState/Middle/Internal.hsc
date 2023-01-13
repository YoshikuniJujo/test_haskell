{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.DepthStencilState.Middle.Internal (
	CreateInfo(..), CreateFlags(..), createInfoToCore
	) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.C.Enum
import Foreign.Pointable
import Control.Monad.Cont
import Data.Bits
import Data.Word

import Gpu.Vulkan.Middle.Internal
import Gpu.Vulkan.Enum
import Gpu.Vulkan.Misc.Middle.Internal

import qualified Gpu.Vulkan.Pipeline.DepthStencilState.Core as C

#include <vulkan/vulkan.h>

enum "CreateFlags" ''#{type VkPipelineDepthStencilStateCreateFlags}
	[''Show, ''Storable, ''Eq, ''Bits] [("CreateFlagsZero", 0)]

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoDepthTestEnable :: Bool,
	createInfoDepthWriteEnable :: Bool,
	createInfoDepthCompareOp :: CompareOp,
	createInfoDepthBoundsTestEnable :: Bool,
	createInfoStencilTestEnable :: Bool,
	createInfoFront :: StencilOpState,
	createInfoBack :: StencilOpState,
	createInfoMinDepthBounds :: Float,
	createInfoMaxDepthBounds :: Float }
	deriving Show

createInfoToCore :: Pointable n => CreateInfo n -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlags flgs,
	createInfoDepthTestEnable = boolToBool32 -> dte,
	createInfoDepthWriteEnable = boolToBool32 -> dwe,
	createInfoDepthCompareOp = CompareOp dco,
	createInfoDepthBoundsTestEnable = boolToBool32 -> bte,
	createInfoStencilTestEnable = boolToBool32 -> ste,
	createInfoFront = stencilOpStateToCore -> fr,
	createInfoBack = stencilOpStateToCore -> bk,
	createInfoMinDepthBounds = mndb,
	createInfoMaxDepthBounds = mxdb
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	let	C.CreateInfo_ fCreateInfo = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt,
			C.createInfoFlags = flgs,
			C.createInfoDepthTestEnable = dte,
			C.createInfoDepthWriteEnable = dwe,
			C.createInfoDepthCompareOp = dco,
			C.createInfoDepthBoundsTestEnable = bte,
			C.createInfoStencilTestEnable = ste,
			C.createInfoFront = fr,
			C.createInfoBack = bk,
			C.createInfoMinDepthBounds = mndb,
			C.createInfoMaxDepthBounds = mxdb }
	ContT $ withForeignPtr fCreateInfo
