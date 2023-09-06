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

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: I.PipelineShaderStageCreateFlags,
	createInfoStage :: ShaderStageFlagBits,
	createInfoModule :: ShaderModule,
	createInfoName :: String,
	createInfoSpecializationInfo :: Maybe SpecializationInfo }
	deriving Show

createInfoToC :: Pointable n => CreateInfo n -> (I.CreateInfo -> IO a) -> IO a
createInfoToC CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoStage = stg,
	createInfoModule = mdl,
	createInfoName = nm,
	createInfoSpecializationInfo = msi } = runContT do
	(castPtr -> pnxt) <- ContT $ withPointerMaybe mnxt
	cnm <- ContT $ withCString utf8 nm
	psi <- case msi of
		Nothing -> pure NullPtr
		Just si -> do
			I.SpecializationInfo_ fsi <-
				ContT $ specializationInfoToC si
			ContT $ withForeignPtr fsi
	pure I.CreateInfo {
		I.createInfoSType = (),
		I.createInfoPNext = pnxt,
		I.createInfoFlags = flgs,
		I.createInfoStage = stg,
		I.createInfoModule = mdl,
		I.createInfoPName = cnm,
		I.createInfoPSpecializationInfo = psi }
