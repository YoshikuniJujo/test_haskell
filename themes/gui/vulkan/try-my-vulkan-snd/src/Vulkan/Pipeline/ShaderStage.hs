{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.ShaderStage where

import Foreign.Pointable

import qualified Data.ByteString as BS

import Vulkan.Pipeline.ShaderStage.Enum

import qualified Vulkan.Device.Type as Device
import qualified Vulkan.Shader.Module as Shader.Module
import qualified Vulkan.Shader.Stage.Enum as Shader.Stage
import qualified Vulkan.Pipeline.ShaderStage.Middle as M

data CreateInfo n n' sknd a a' vs = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoStage :: Shader.Stage.FlagBits,
	createInfoModule :: Shader.Module.M n' sknd a a',
	createInfoName :: BS.ByteString,
	createInfoSpecializationInfo :: Maybe vs }

createInfoToMiddle :: (Pointable n', Pointable a) =>
	Device.D ds -> CreateInfo n n' sknd a a' vs -> IO (M.CreateInfo n sknd vs)
createInfoToMiddle dvc CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoStage = stg,
	createInfoModule = mdl,
	createInfoName = nm,
	createInfoSpecializationInfo = spi
	} = do
	mdl' <- Shader.Module.create dvc mdl
	pure M.CreateInfo {
		M.createInfoNext = mnxt,
		M.createInfoFlags = flgs,
		M.createInfoStage = stg,
		M.createInfoModule = mdl',
		M.createInfoName = nm,
		M.createInfoSpecializationInfo = spi }

destroyCreateInfoMiddle :: Pointable a' =>
	Device.D ds ->
	M.CreateInfo n sknd vs -> CreateInfo n n' sknd a a' vs -> IO ()
destroyCreateInfoMiddle dvc
	M.CreateInfo { M.createInfoModule = mmdl } 
	CreateInfo { createInfoModule = mdl } = Shader.Module.destroy dvc mmdl mdl
