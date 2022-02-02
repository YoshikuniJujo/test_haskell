{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.RasterizationState where

import Foreign.Ptr
import Control.Monad.Cont

import Vulkan.Base
import Vulkan.PolygonMode
import Vulkan.CullModeFlagBits
import Vulkan.FrontFace

import qualified Vulkan.Pipeline.RasterizationState.Internal as I

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: I.CreateFlags,
	createInfoDepthClampEnable :: Bool,
	createInfoRasterizerDiscardEnable :: Bool,
	createInfoPolygonMode :: PolygonMode,
	createInfoCullMode :: CullModeFlags,
	createInfoFrontFace :: FrontFace,
	createInfoDepthBiasEnable :: Bool,
	createInfoDepthBiasConstantFactor :: Float,
	createInfoDepthBiasClamp :: Float,
	createInfoDepthBiasSlopeFactor :: Float,
	createInfoLineWidth :: Float }
	deriving Show

createInfoToC :: Pointable n => CreateInfo n -> (I.CreateInfo -> IO a) -> IO a
createInfoToC CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoDepthClampEnable = (boolToBool32 -> dce),
	createInfoRasterizerDiscardEnable = (boolToBool32 -> rde),
	createInfoPolygonMode = pm,
	createInfoCullMode = cm,
	createInfoFrontFace = ff,
	createInfoDepthBiasEnable = (boolToBool32 -> dbe),
	createInfoDepthBiasConstantFactor = (floatToFloat -> dbcf),
	createInfoDepthBiasClamp = (floatToFloat -> dbc),
	createInfoDepthBiasSlopeFactor = (floatToFloat -> dbsf),
	createInfoLineWidth = (floatToFloat -> lw) } = runContT do
	(castPtr -> pnxt) <- ContT $ withMaybePointer mnxt
	pure I.CreateInfo {
		I.createInfoSType = (),
		I.createInfoFlags = flgs,
		I.createInfoPNext = pnxt,
		I.createInfoDepthClampEnable = dce,
		I.createInfoRasterizerDiscardEnable = rde,
		I.createInfoPolygonMode = pm,
		I.createInfoCullMode = cm,
		I.createInfoFrontFace = ff,
		I.createInfoDepthBiasEnable = dbe,
		I.createInfoDepthBiasConstantFactor = dbcf,
		I.createInfoDepthBiasClamp = dbc,
		I.createInfoDepthBiasSlopeFactor = dbsf,
		I.createInfoLineWidth = lw }
