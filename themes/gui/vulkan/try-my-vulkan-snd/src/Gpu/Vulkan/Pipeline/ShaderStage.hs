{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.ShaderStage where

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

data CreateInfo n n' sknd c d vs = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoStage :: ShaderStageFlagBits,
	createInfoModule :: Shader.Module.M n' sknd c d,
	createInfoName :: BS.ByteString,
	createInfoSpecializationInfo :: Maybe vs }

type CreateInfo' = V6 CreateInfo

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

class CreateInfoToMiddle nnskndcdvs where
	type Result nnskndcdvs
	createInfoToMiddle' :: Device.D ds -> CreateInfo' nnskndcdvs ->
		IO (Result nnskndcdvs)

instance (Pointable n', Pointable c) => CreateInfoToMiddle '(n, n', sknd, c, d, vs) where
	type Result '(n, n', sknd, c, d, vs) = M.CreateInfo n sknd vs
	createInfoToMiddle' dvc (V6 ci) = createInfoToMiddle dvc ci

destroyCreateInfoMiddle :: Pointable d => Device.D ds ->
	M.CreateInfo n sknd vs -> CreateInfo n n' sknd c d vs -> IO ()
destroyCreateInfoMiddle dvc
	M.CreateInfo { M.createInfoModule = mmdl } 
	CreateInfo { createInfoModule = mdl } = Shader.Module.destroy dvc mmdl mdl

class CreateInfoListToMiddle' (
	nnskndcdvss :: [(Type, Type, ShaderKind, Type, Type, Type)]
	) where
	type MiddleVars nnskndcdvss :: [(Type, ShaderKind, Type)]
	createInfoListToMiddle' :: Device.D ds ->
		HeteroVarList CreateInfo' nnskndcdvss ->
		IO (HeteroVarList M.CreateInfo' (MiddleVars nnskndcdvss))
	destroyCreateInfoMiddleList' :: Device.D ds ->
		HeteroVarList M.CreateInfo' (MiddleVars nnskndcdvss) ->
		HeteroVarList CreateInfo' nnskndcdvss -> IO ()

instance CreateInfoListToMiddle' '[] where
	type MiddleVars '[] = '[]
	createInfoListToMiddle' _ HVNil = pure HVNil
	destroyCreateInfoMiddleList' _ HVNil HVNil = pure ()

instance (
	Pointable n', Pointable c, Pointable d,
	CreateInfoListToMiddle' nnskndcdvss ) =>
	CreateInfoListToMiddle' ('(n, n', sknd, c, d, vs) ': nnskndcdvss) where
	type MiddleVars ('(n, n', sknd, c, d, vs) ': nnskndcdvss) =
		'(n, sknd, vs) ': MiddleVars nnskndcdvss
	createInfoListToMiddle' dvc (V6 ci :...: cis) = (:...:)
		<$> (V3 <$> createInfoToMiddle dvc ci)
		<*> createInfoListToMiddle' dvc cis
	destroyCreateInfoMiddleList' dvc (V3 cim :...: cims) (V6 ci :...: cis) =
		destroyCreateInfoMiddle dvc cim ci >>
		destroyCreateInfoMiddleList' dvc cims cis

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
