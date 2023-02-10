{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.ShaderStage.Internal (
	CreateInfoNew(..), CreateInfoListToMiddleNew(..),
	createInfoToMiddleFooNew,
	destroyCreateInfoMiddleNew
	) where

import Foreign.Storable.PeekPoke
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

data CreateInfoNew n m sknd c d vs = CreateInfoNew {
	createInfoNextNew :: Maybe n,
	createInfoFlagsNew :: CreateFlags,
	createInfoStageNew :: ShaderStageFlagBits,
	createInfoModuleNew :: Shader.Module.M m sknd c d,
	createInfoNameNew :: BS.ByteString,
	createInfoSpecializationInfoNew :: Maybe (HeteroList' vs) }

createInfoToMiddleNew :: (Pointable m, Pokable c) =>
	Device.D ds -> CreateInfoNew n m sknd c d vs -> IO (M.CreateInfo n sknd vs)
createInfoToMiddleNew dvc CreateInfoNew {
	createInfoNextNew = mnxt,
	createInfoFlagsNew = flgs,
	createInfoStageNew = stg,
	createInfoModuleNew = mdl,
	createInfoNameNew = nm,
	createInfoSpecializationInfoNew = spi
	} = do
	mdl' <- Shader.Module.create dvc mdl
	pure M.CreateInfo {
		M.createInfoNext = mnxt,
		M.createInfoFlags = flgs,
		M.createInfoStage = stg,
		M.createInfoModule = mdl',
		M.createInfoName = nm,
		M.createInfoSpecializationInfo = spi }

createInfoToMiddleFooNew :: (Pointable m, Pokable c) => Device.D ds ->
	V6 CreateInfoNew '(n, m, sknd, c, d, vs) -> IO (M.CreateInfo n sknd vs)
createInfoToMiddleFooNew dvc (V6 ci) = createInfoToMiddleNew dvc ci

destroyCreateInfoMiddleNew :: Pokable d => Device.D ds ->
	M.CreateInfo n sknd vs -> CreateInfoNew n m sknd c d vs -> IO ()
destroyCreateInfoMiddleNew dvc
	M.CreateInfo { M.createInfoModule = mmdl }
	CreateInfoNew { createInfoModuleNew = mdl } = Shader.Module.destroy dvc mmdl mdl

class CreateInfoListToMiddleNew (
	nnskndcdvss :: [(Type, Type, ShaderKind, Type, Type, [Type])]
	) where
	type MiddleVarsNew nnskndcdvss :: [(Type, ShaderKind, [Type])]
	createInfoListToMiddleNew :: Device.D ds ->
		HeteroVarList (V6 CreateInfoNew) nnskndcdvss ->
		IO (HeteroVarList (V3 M.CreateInfo) (MiddleVarsNew nnskndcdvss))
	destroyCreateInfoMiddleListNew :: Device.D ds ->
		HeteroVarList (V3 M.CreateInfo) (MiddleVarsNew nnskndcdvss) ->
		HeteroVarList (V6 CreateInfoNew) nnskndcdvss -> IO ()

instance CreateInfoListToMiddleNew '[] where
	type MiddleVarsNew '[] = '[]
	createInfoListToMiddleNew _ HVNil = pure HVNil
	destroyCreateInfoMiddleListNew _ HVNil HVNil = pure ()

instance (
	Pointable m, Pokable c, Pokable d,
	CreateInfoListToMiddleNew nnskndcdvss ) =>
	CreateInfoListToMiddleNew ('(n, m, sknd, c, d, vs) ': nnskndcdvss) where
	type MiddleVarsNew ('(n, m, sknd, c, d, vs) ': nnskndcdvss) =
		'(n, sknd, vs) ': MiddleVarsNew nnskndcdvss
	createInfoListToMiddleNew dvc (V6 ci :...: cis) = (:...:)
		<$> (V3 <$> createInfoToMiddleNew dvc ci)
		<*> createInfoListToMiddleNew dvc cis
	destroyCreateInfoMiddleListNew dvc (V3 cim :...: cims) (V6 ci :...: cis) =
		destroyCreateInfoMiddleNew dvc cim ci >>
		destroyCreateInfoMiddleListNew dvc cims cis
