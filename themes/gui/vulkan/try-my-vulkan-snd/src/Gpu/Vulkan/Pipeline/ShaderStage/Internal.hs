{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.ShaderStage.Internal (
	CreateInfoNew(..), CreateInfoListToMiddleNew(..), DestroyCreateInfoMiddleListNew'(..),
	createInfoToMiddleFooNew,
	destroyCreateInfoMiddleNew,

	allocationCallbacksListFromCreateInfoList
	) where

import Foreign.Storable.PeekPoke
import Data.Kind
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.MapIndex qualified as TMapIndex
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:**))
import Data.HeteroParList.Tuple qualified as HeteroParList

import Shaderc.EnumAuto

import qualified Data.ByteString as BS

import Gpu.Vulkan.Enum
import Gpu.Vulkan.AllocationCallbacks.Type qualified as AllocationCallbacks
import Gpu.Vulkan.Pipeline.ShaderStage.Enum

import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.ShaderModule.Internal as Shader.Module
import qualified Gpu.Vulkan.Pipeline.ShaderStage.Middle as M

data CreateInfoNew mn m sknd mscc vs = CreateInfoNew {
	createInfoNextNew :: TMaybe.M mn,
	createInfoFlagsNew :: CreateFlags,
	createInfoStageNew :: ShaderStageFlagBits,
	createInfoModuleNew :: (
		Shader.Module.CreateInfo m sknd, TPMaybe.M (U2 AllocationCallbacks.A) mscc ),
	createInfoNameNew :: BS.ByteString,
	createInfoSpecializationInfoNew :: Maybe (HeteroParList.L vs) }

allocationCallbacksFromCreateInfo ::
	CreateInfoNew mn m sknd mscc vs ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc
allocationCallbacksFromCreateInfo
	CreateInfoNew { createInfoModuleNew = (_, mac) } = mac

allocationCallbacksListFromCreateInfoList :: HeteroParList.Map3_5 cias =>
	HeteroParList.PL (U5 CreateInfoNew) cias ->
	HeteroParList.PL
		(TPMaybe.M (U2 AllocationCallbacks.A)) (TMapIndex.M3_5 cias)
allocationCallbacksListFromCreateInfoList =
	HeteroParList.map3_5 (allocationCallbacksFromCreateInfo . unU5)

createInfoToMiddleNew ::
	(WithPoked (TMaybe.M m), AllocationCallbacks.ToMiddle mac)  =>
	Device.D ds -> CreateInfoNew n m sknd mac vs ->
	IO (M.CreateInfo n sknd vs)
createInfoToMiddleNew dvc CreateInfoNew {
	createInfoNextNew = mnxt,
	createInfoFlagsNew = flgs,
	createInfoStageNew = stg,
	createInfoModuleNew = (mdl, mac),
	createInfoNameNew = nm,
	createInfoSpecializationInfoNew = spi
	} = do
	mdl' <- Shader.Module.create dvc mdl mac
	pure M.CreateInfo {
		M.createInfoNext = mnxt,
		M.createInfoFlags = flgs,
		M.createInfoStage = stg,
		M.createInfoModule = mdl',
		M.createInfoName = nm,
		M.createInfoSpecializationInfo = spi }

createInfoToMiddleFooNew ::
	(WithPoked (TMaybe.M m), AllocationCallbacks.ToMiddle mscc) =>
	Device.D ds ->
	U5 CreateInfoNew '(n, m, sknd, mscc, vs) -> IO (M.CreateInfo n sknd vs)
createInfoToMiddleFooNew dvc (U5 ci) = createInfoToMiddleNew dvc ci

destroyCreateInfoMiddleNew :: AllocationCallbacks.ToMiddle mscc =>
	Device.D ds ->
	M.CreateInfo n sknd vs ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc -> IO ()
destroyCreateInfoMiddleNew dvc
	M.CreateInfo { M.createInfoModule = mmdl }
	mac = Shader.Module.destroy dvc mmdl mac

class DestroyCreateInfoMiddleListNew' (MiddleVarsNew nnskndcdvss) (TMapIndex.M3_5 nnskndcdvss) =>
	CreateInfoListToMiddleNew (
	nnskndcdvss :: [(Maybe Type, Maybe Type, ShaderKind, Maybe (Type, Type), [Type])]
	) where
	type MiddleVarsNew nnskndcdvss :: [(Maybe Type, ShaderKind, [Type])]
	createInfoListToMiddleNew ::
		Device.D ds ->
		HeteroParList.PL (U5 CreateInfoNew) nnskndcdvss ->
		IO (HeteroParList.PL (U3 M.CreateInfo) (MiddleVarsNew nnskndcdvss))

instance CreateInfoListToMiddleNew '[] where
	type MiddleVarsNew '[] = '[]
	createInfoListToMiddleNew _ HeteroParList.Nil = pure HeteroParList.Nil

instance (
	WithPoked (TMaybe.M m),
	CreateInfoListToMiddleNew nnskndcdvss,

	AllocationCallbacks.ToMiddle mscc
	) =>
	CreateInfoListToMiddleNew ('(n, m, sknd, mscc, vs) ': nnskndcdvss) where
	type MiddleVarsNew ('(n, m, sknd, mscc, vs) ': nnskndcdvss) =
		'(n, sknd, vs) ': MiddleVarsNew nnskndcdvss
	createInfoListToMiddleNew dvc (U5 ci :** cis) = (:**)
		<$> (U3 <$> createInfoToMiddleNew dvc ci)
		<*> createInfoListToMiddleNew dvc cis

class DestroyCreateInfoMiddleListNew' (foo :: [(Maybe Type, ShaderKind, [Type])]) (macs :: [Maybe (Type, Type)])  where
	destroyCreateInfoMiddleListNew' ::
		Device.D ds ->
		HeteroParList.PL (U3 M.CreateInfo) foo ->
		HeteroParList.PL (TPMaybe.M (U2 AllocationCallbacks.A)) macs -> IO ()

instance DestroyCreateInfoMiddleListNew' '[] '[] where
	destroyCreateInfoMiddleListNew' _ HeteroParList.Nil HeteroParList.Nil = pure ()

instance (
	AllocationCallbacks.ToMiddle mac,
	DestroyCreateInfoMiddleListNew' foos macs ) =>
	DestroyCreateInfoMiddleListNew' (foo ': foos) (mac ': macs) where
	destroyCreateInfoMiddleListNew' dvc (U3 cim :** cims) (mac :** macs) =
		destroyCreateInfoMiddleNew dvc cim mac >>
		destroyCreateInfoMiddleListNew' dvc cims macs
