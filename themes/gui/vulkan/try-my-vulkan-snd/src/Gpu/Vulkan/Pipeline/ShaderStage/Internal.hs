{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.ShaderStage.Internal (
	CreateInfoNew(..), CreateInfoListToMiddleNew(..),
	createInfoToMiddleFooNew,
	destroyCreateInfoMiddleNew
	) where

import Foreign.Storable.PeekPoke
import Data.Kind
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.Uncurry
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:*), pattern (:**))

import Shaderc.EnumAuto

import qualified Data.ByteString as BS

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Pipeline.ShaderStage.Enum

import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.ShaderModule.Internal as Shader.Module
import qualified Gpu.Vulkan.Pipeline.ShaderStage.Middle as M

data CreateInfoNew mn m sknd c d vs = CreateInfoNew {
	createInfoNextNew :: TMaybe.M mn,
	createInfoFlagsNew :: CreateFlags,
	createInfoStageNew :: ShaderStageFlagBits,
	createInfoModuleNew :: Shader.Module.M m sknd c d,
	createInfoNameNew :: BS.ByteString,
	createInfoSpecializationInfoNew :: Maybe (HeteroParList.L vs) }

createInfoToMiddleNew :: (Pokable m, Pokable c) =>
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

createInfoToMiddleFooNew :: (Pokable m, Pokable c) => Device.D ds ->
	U6 CreateInfoNew '(n, m, sknd, c, d, vs) -> IO (M.CreateInfo n sknd vs)
createInfoToMiddleFooNew dvc (U6 ci) = createInfoToMiddleNew dvc ci

destroyCreateInfoMiddleNew :: Pokable d => Device.D ds ->
	M.CreateInfo n sknd vs -> CreateInfoNew n m sknd c d vs -> IO ()
destroyCreateInfoMiddleNew dvc
	M.CreateInfo { M.createInfoModule = mmdl }
	CreateInfoNew { createInfoModuleNew = mdl } = Shader.Module.destroy dvc mmdl mdl

class CreateInfoListToMiddleNew (
	nnskndcdvss :: [(Maybe Type, Type, ShaderKind, Type, Type, [Type])]
	) where
	type MiddleVarsNew nnskndcdvss :: [(Maybe Type, ShaderKind, [Type])]
	createInfoListToMiddleNew :: Device.D ds ->
		HeteroParList.PL (U6 CreateInfoNew) nnskndcdvss ->
		IO (HeteroParList.PL (U3 M.CreateInfo) (MiddleVarsNew nnskndcdvss))
	destroyCreateInfoMiddleListNew :: Device.D ds ->
		HeteroParList.PL (U3 M.CreateInfo) (MiddleVarsNew nnskndcdvss) ->
		HeteroParList.PL (U6 CreateInfoNew) nnskndcdvss -> IO ()

instance CreateInfoListToMiddleNew '[] where
	type MiddleVarsNew '[] = '[]
	createInfoListToMiddleNew _ HeteroParList.Nil = pure HeteroParList.Nil
	destroyCreateInfoMiddleListNew _ HeteroParList.Nil HeteroParList.Nil = pure ()

instance (
	Pokable m, Pokable c, Pokable d,
	CreateInfoListToMiddleNew nnskndcdvss ) =>
	CreateInfoListToMiddleNew ('(n, m, sknd, c, d, vs) ': nnskndcdvss) where
	type MiddleVarsNew ('(n, m, sknd, c, d, vs) ': nnskndcdvss) =
		'(n, sknd, vs) ': MiddleVarsNew nnskndcdvss
	createInfoListToMiddleNew dvc (U6 ci :** cis) = (:**)
		<$> (U3 <$> createInfoToMiddleNew dvc ci)
		<*> createInfoListToMiddleNew dvc cis
	destroyCreateInfoMiddleListNew dvc (U3 cim :** cims) (U6 ci :** cis) =
		destroyCreateInfoMiddleNew dvc cim ci >>
		destroyCreateInfoMiddleListNew dvc cims cis
