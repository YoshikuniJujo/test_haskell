{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.ShaderStage.Internal (
	CreateInfo(..), CreateInfo', CreateInfoListToMiddle'(..),
	createInfoToMiddleFoo,
	destroyCreateInfoMiddle,

	CreateInfoNew(..),
	createInfoToMiddleFooNew,
	destroyCreateInfoMiddleNew
	) where

import Foreign.Pointable
import Data.Kind
import Data.HeteroList

import Shaderc.EnumAuto

import qualified Data.ByteString as BS

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Pipeline.ShaderStage.Enum

import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.ShaderModule.Internal as Shader.Module
import qualified Gpu.Vulkan.Pipeline.ShaderStage.Middle as M

data CreateInfo n m sknd c d vs = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoStage :: ShaderStageFlagBits,
	createInfoModule :: Shader.Module.M m sknd c d,
	createInfoName :: BS.ByteString,
	createInfoSpecializationInfo :: Maybe vs }

data CreateInfoNew n m sknd c d vs = CreateInfoNew {
	createInfoNextNew :: Maybe n,
	createInfoFlagsNew :: CreateFlags,
	createInfoStageNew :: ShaderStageFlagBits,
	createInfoModuleNew :: Shader.Module.M m sknd c d,
	createInfoNameNew :: BS.ByteString,
	createInfoSpecializationInfoNew :: Maybe (HeteroList' vs) }

type CreateInfo' = V6 CreateInfo

createInfoToMiddle :: (Pointable m, Pointable c) =>
	Device.D ds -> CreateInfo n m sknd c d vs -> IO (M.CreateInfo n sknd vs)
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

createInfoToMiddleNew :: (Pointable m, Pointable c) =>
	Device.D ds -> CreateInfoNew n m sknd c d vs -> IO (M.CreateInfoNew n sknd vs)
createInfoToMiddleNew dvc CreateInfoNew {
	createInfoNextNew = mnxt,
	createInfoFlagsNew = flgs,
	createInfoStageNew = stg,
	createInfoModuleNew = mdl,
	createInfoNameNew = nm,
	createInfoSpecializationInfoNew = spi
	} = do
	mdl' <- Shader.Module.create dvc mdl
	pure M.CreateInfoNew {
		M.createInfoNextNew = mnxt,
		M.createInfoFlagsNew = flgs,
		M.createInfoStageNew = stg,
		M.createInfoModuleNew = mdl',
		M.createInfoNameNew = nm,
		M.createInfoSpecializationInfoNew = spi }

createInfoToMiddleFoo :: (Pointable m, Pointable c) => Device.D ds ->
	CreateInfo' '(n, m, sknd, c, d, vs) -> IO (M.CreateInfo n sknd vs)
createInfoToMiddleFoo dvc (V6 ci) = createInfoToMiddle dvc ci

createInfoToMiddleFooNew :: (Pointable m, Pointable c) => Device.D ds ->
	V6 CreateInfoNew '(n, m, sknd, c, d, vs) -> IO (M.CreateInfoNew n sknd vs)
createInfoToMiddleFooNew dvc (V6 ci) = createInfoToMiddleNew dvc ci

destroyCreateInfoMiddle :: Pointable d => Device.D ds ->
	M.CreateInfo n sknd vs -> CreateInfo n m sknd c d vs -> IO ()
destroyCreateInfoMiddle dvc
	M.CreateInfo { M.createInfoModule = mmdl }
	CreateInfo { createInfoModule = mdl } = Shader.Module.destroy dvc mmdl mdl

destroyCreateInfoMiddleNew :: Pointable d => Device.D ds ->
	M.CreateInfoNew n sknd vs -> CreateInfoNew n m sknd c d vs -> IO ()
destroyCreateInfoMiddleNew dvc
	M.CreateInfoNew { M.createInfoModuleNew = mmdl }
	CreateInfoNew { createInfoModuleNew = mdl } = Shader.Module.destroy dvc mmdl mdl

class CreateInfoListToMiddle' (
	nnskndcdvss :: [(Type, Type, ShaderKind, Type, Type, Type)]
	) where
	type MiddleVars nnskndcdvss :: [(Type, ShaderKind, Type)]
	createInfoListToMiddle' :: Device.D ds ->
		HeteroVarList CreateInfo' nnskndcdvss ->
		IO (HeteroVarList (V3 M.CreateInfo) (MiddleVars nnskndcdvss))
	destroyCreateInfoMiddleList' :: Device.D ds ->
		HeteroVarList (V3 M.CreateInfo) (MiddleVars nnskndcdvss) ->
		HeteroVarList CreateInfo' nnskndcdvss -> IO ()

instance CreateInfoListToMiddle' '[] where
	type MiddleVars '[] = '[]
	createInfoListToMiddle' _ HVNil = pure HVNil
	destroyCreateInfoMiddleList' _ HVNil HVNil = pure ()

instance (
	Pointable m, Pointable c, Pointable d,
	CreateInfoListToMiddle' nnskndcdvss ) =>
	CreateInfoListToMiddle' ('(n, m, sknd, c, d, vs) ': nnskndcdvss) where
	type MiddleVars ('(n, m, sknd, c, d, vs) ': nnskndcdvss) =
		'(n, sknd, vs) ': MiddleVars nnskndcdvss
	createInfoListToMiddle' dvc (V6 ci :...: cis) = (:...:)
		<$> (V3 <$> createInfoToMiddle dvc ci)
		<*> createInfoListToMiddle' dvc cis
	destroyCreateInfoMiddleList' dvc (V3 cim :...: cims) (V6 ci :...: cis) =
		destroyCreateInfoMiddle dvc cim ci >>
		destroyCreateInfoMiddleList' dvc cims cis
