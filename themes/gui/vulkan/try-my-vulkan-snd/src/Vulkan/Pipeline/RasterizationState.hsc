{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.RasterizationState where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.C.Enum
import Foreign.Pointable
import Control.Monad.Cont
import Data.Bits
import Data.Word

import Vulkan.Base
import Vulkan.Enum

import qualified Vulkan.CullMode.Enum as CullMode
import qualified Vulkan.Pipeline.RasterizationState.Core as C

#include <vulkan/vulkan.h>

enum "CreateFlags" ''#{type VkPipelineRasterizationStateCreateFlags}
	[''Show, ''Eq, ''Storable, ''Bits] [("CreateFlagsZero", 0)]

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoDepthClampEnable :: Bool,
	createInfoRasterizerDiscardEnable :: Bool,
	createInfoPolygonMode :: PolygonMode,
	createInfoCullMode :: CullMode.Flags,
	createInfoFrontFace :: FrontFace,
	createInfoDepthBiasEnable :: Bool,
	createInfoDepthBiasConstantFactor :: Float,
	createInfoDepthBiasClamp :: Float,
	createInfoDepthBiasSlopeFactor :: Float,
	createInfoLineWidth :: Float }
	deriving Show

createInfoToCore :: Pointable n => CreateInfo n -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlags flgs,
	createInfoDepthClampEnable = boolToBool32 -> dce,
	createInfoRasterizerDiscardEnable = boolToBool32 -> rde,
	createInfoPolygonMode = PolygonMode pm,
	createInfoCullMode = CullMode.FlagBits cm,
	createInfoFrontFace = FrontFace ff,
	createInfoDepthBiasEnable = boolToBool32 -> dbe,
	createInfoDepthBiasConstantFactor = dbcf,
	createInfoDepthBiasClamp = dbc,
	createInfoDepthBiasSlopeFactor = dbsf,
	createInfoLineWidth = lw } = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	let	C.CreateInfo_ fCreateInfo = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt,
			C.createInfoFlags = flgs,
			C.createInfoDepthClampEnable = dce,
			C.createInfoRasterizerDiscardEnable = rde,
			C.createInfoPolygonMode = pm,
			C.createInfoCullMode = cm,
			C.createInfoFrontFace = ff,
			C.createInfoDepthBiasEnable = dbe,
			C.createInfoDepthBiasConstantFactor = dbcf,
			C.createInfoDepthBiasClamp = dbc,
			C.createInfoDepthBiasSlopeFactor = dbsf,
			C.createInfoLineWidth = lw }
	ContT $ withForeignPtr fCreateInfo
