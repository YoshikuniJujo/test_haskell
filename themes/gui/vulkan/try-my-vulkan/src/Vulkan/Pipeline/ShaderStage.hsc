{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.ShaderStage where

import GHC.Foreign
import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Monad.Cont
import System.IO

import Vulkan.Base
import Vulkan.ShaderStageFlagBits
import Vulkan.SpecializationInfo
import Vulkan.Shader

import qualified Vulkan.SpecializationInfo.Internal as I
import qualified Vulkan.Pipeline.ShaderStage.Internal as I

data PipelineShaderStageCreateInfo n = PipelineShaderStageCreateInfo {
	pipelineShaderStageCreateInfoNext :: Maybe n,
	pipelineShaderStageCreateInfoFlags :: I.PipelineShaderStageCreateFlags,
	pipelineShaderStageCreateInfoStage :: ShaderStageFlagBits,
	pipelineShaderStageCreateInfoModule :: ShaderModule,
	pipelineShaderStageCreateInfoName :: String,
	pipelineShaderStageCreateInfoSpecializationInfo ::
		Maybe SpecializationInfo }
	deriving Show

pipelineShaderStageCreateInfoToC :: Pointable n =>
	PipelineShaderStageCreateInfo n ->
	(I.PipelineShaderStageCreateInfo -> IO a) -> IO a
pipelineShaderStageCreateInfoToC PipelineShaderStageCreateInfo {
	pipelineShaderStageCreateInfoNext = mnxt,
	pipelineShaderStageCreateInfoFlags = flgs,
	pipelineShaderStageCreateInfoStage = stg,
	pipelineShaderStageCreateInfoModule = mdl,
	pipelineShaderStageCreateInfoName = nm,
	pipelineShaderStageCreateInfoSpecializationInfo = msi } = runContT do
	(castPtr -> pnxt) <- ContT $ withPointerMaybe mnxt
	cnm <- ContT $ withCString utf8 nm
	psi <- case msi of
		Nothing -> pure NullPtr
		Just si -> do
			I.SpecializationInfo_ fsi <-
				ContT $ specializationInfoToC si
			ContT $ withForeignPtr fsi
	pure I.PipelineShaderStageCreateInfo {
		I.pipelineShaderStageCreateInfoSType = (),
		I.pipelineShaderStageCreateInfoPNext = pnxt,
		I.pipelineShaderStageCreateInfoFlags = flgs,
		I.pipelineShaderStageCreateInfoStage = stg,
		I.pipelineShaderStageCreateInfoModule = mdl,
		I.pipelineShaderStageCreateInfoPName = cnm,
		I.pipelineShaderStageCreateInfoPSpecializationInfo = psi }
