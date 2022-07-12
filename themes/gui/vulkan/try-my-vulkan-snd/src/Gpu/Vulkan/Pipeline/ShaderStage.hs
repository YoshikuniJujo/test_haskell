{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.ShaderStage where

import Foreign.Pointable

import qualified Data.ByteString as BS

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Pipeline.ShaderStage.Enum

import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.ShaderModule.Internal as Shader.Module
import qualified Gpu.Vulkan.Pipeline.ShaderStage.Middle as M

data CreateInfo n n' sknd c d vs = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoStage :: ShaderStageFlagBits,
	createInfoModule :: Shader.Module.M n' sknd c d,
	createInfoName :: BS.ByteString,
	createInfoSpecializationInfo :: Maybe vs }

createInfoToMiddle :: (Pointable n', Pointable c) =>
	Device.D ds -> CreateInfo n n' sknd c d vs -> IO (M.CreateInfo n sknd vs)
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

destroyCreateInfoMiddle :: Pointable d => Device.D ds ->
	M.CreateInfo n sknd vs -> CreateInfo n n' sknd c d vs -> IO ()
destroyCreateInfoMiddle dvc
	M.CreateInfo { M.createInfoModule = mmdl } 
	CreateInfo { createInfoModule = mdl } = Shader.Module.destroy dvc mmdl mdl

infixr 5 `CreateInfoCons`

data CreateInfoList n n' sknds vss a a' where
	CreateInfoNil :: CreateInfoList n n' '[] a a' '[]
	CreateInfoCons :: CreateInfo n n' sknd a a' vs ->
		CreateInfoList n n' sknds a a' vss ->
		CreateInfoList n n' (sknd ': sknds) a a' (vs ': vss)

createInfoListToMiddle :: (Pointable n', Pointable a) =>
	Device.D ds ->
	CreateInfoList n n' sknds a a' vss -> IO (M.CreateInfoList n sknds vss)
createInfoListToMiddle _ CreateInfoNil = pure M.CreateInfoNil
createInfoListToMiddle dvc (ci `CreateInfoCons` cis) = M.CreateInfoCons
	<$> createInfoToMiddle dvc ci
	<*> createInfoListToMiddle dvc cis

destroyCreateInfoMiddleList :: Pointable a' =>
	Device.D ds ->
	M.CreateInfoList n sknds vss -> CreateInfoList n n' sknds a a' vss ->
	IO ()
destroyCreateInfoMiddleList _ M.CreateInfoNil CreateInfoNil = pure ()
destroyCreateInfoMiddleList dvc
	(mci `M.CreateInfoCons` mcis) (ci `CreateInfoCons` cis) = do
	destroyCreateInfoMiddle dvc mci ci
	destroyCreateInfoMiddleList dvc mcis cis
