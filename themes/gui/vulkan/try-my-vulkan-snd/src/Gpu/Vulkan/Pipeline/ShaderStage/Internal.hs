{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.ShaderStage.Internal (
	CreateInfo(..), CreateInfoListToMiddle(..), DestroyShaderModuleList(..),
	createInfoToMiddleFooNew,
	destroyShaderModule,

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
import qualified Gpu.Vulkan.ShaderModule.Internal as ShaderModule
import qualified Gpu.Vulkan.Pipeline.ShaderStage.Middle as M

data CreateInfo mn mnsm sknd mac vs = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoStage :: ShaderStageFlagBits,
	createInfoModule :: (
		ShaderModule.CreateInfo mnsm sknd, TPMaybe.M (U2 AllocationCallbacks.A) mac ),
	createInfoName :: BS.ByteString,
	createInfoSpecializationInfo :: Maybe (HeteroParList.L vs) }

allocationCallbacksFromCreateInfo ::
	CreateInfo mn mnsm sknd mac vs ->
	TPMaybe.M (U2 AllocationCallbacks.A) mac
allocationCallbacksFromCreateInfo CreateInfo { createInfoModule = (_, mac) } =
	mac

allocationCallbacksListFromCreateInfoList :: HeteroParList.Map3_5 cias =>
	HeteroParList.PL (U5 CreateInfo) cias ->
	HeteroParList.PL
		(TPMaybe.M (U2 AllocationCallbacks.A)) (TMapIndex.M3_5 cias)
allocationCallbacksListFromCreateInfoList =
	HeteroParList.map3_5 (allocationCallbacksFromCreateInfo . unU5)

createInfoToMiddle ::
	(WithPoked (TMaybe.M mnsm), AllocationCallbacks.ToMiddle mac)  =>
	Device.D ds -> CreateInfo n mnsm sknd mac vs ->
	IO (M.CreateInfo n sknd vs)
createInfoToMiddle dvc CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoStage = stg,
	createInfoModule = (mdl, mac),
	createInfoName = nm,
	createInfoSpecializationInfo = spi
	} = do
	mdl' <- ShaderModule.create dvc mdl mac
	pure M.CreateInfo {
		M.createInfoNext = mnxt,
		M.createInfoFlags = flgs,
		M.createInfoStage = stg,
		M.createInfoModule = mdl',
		M.createInfoName = nm,
		M.createInfoSpecializationInfo = spi }

createInfoToMiddleFooNew ::
	(WithPoked (TMaybe.M m), AllocationCallbacks.ToMiddle mac) =>
	Device.D ds ->
	U5 CreateInfo '(n, m, sknd, mac, vs) -> IO (M.CreateInfo n sknd vs)
createInfoToMiddleFooNew dvc (U5 ci) = createInfoToMiddle dvc ci

destroyShaderModule :: AllocationCallbacks.ToMiddle mac =>
	Device.D ds ->
	M.CreateInfo n sknd vs ->
	TPMaybe.M (U2 AllocationCallbacks.A) mac -> IO ()
destroyShaderModule dvc
	M.CreateInfo { M.createInfoModule = mmdl }
	mac = ShaderModule.destroy dvc mmdl mac

class DestroyShaderModuleList (MiddleArgs cias) (TMapIndex.M3_5 cias) =>
	CreateInfoListToMiddle cias where
	type MiddleArgs cias :: [(Maybe Type, ShaderKind, [Type])]
	createInfoListToMiddle ::
		Device.D ds ->
		HeteroParList.PL (U5 CreateInfo) cias ->
		IO (HeteroParList.PL (U3 M.CreateInfo) (MiddleArgs cias))

instance CreateInfoListToMiddle '[] where
	type MiddleArgs '[] = '[]
	createInfoListToMiddle _ HeteroParList.Nil = pure HeteroParList.Nil

instance (
	WithPoked (TMaybe.M mnsm), AllocationCallbacks.ToMiddle mac,
	CreateInfoListToMiddle cias ) =>
	CreateInfoListToMiddle ('(n, mnsm, sknd, mac, vs) ': cias) where
	type MiddleArgs ('(n, mnsm, sknd, mac, vs) ': cias) =
		'(n, sknd, vs) ': MiddleArgs cias
	createInfoListToMiddle dvc (U5 ci :** cis) = (:**)
		<$> (U3 <$> createInfoToMiddle dvc ci)
		<*> createInfoListToMiddle dvc cis

class DestroyShaderModuleList (foo :: [(Maybe Type, ShaderKind, [Type])]) (macs :: [Maybe (Type, Type)])  where
	destroyShaderModuleList ::
		Device.D ds ->
		HeteroParList.PL (U3 M.CreateInfo) foo ->
		HeteroParList.PL (TPMaybe.M (U2 AllocationCallbacks.A)) macs -> IO ()

instance DestroyShaderModuleList '[] '[] where
	destroyShaderModuleList _ HeteroParList.Nil HeteroParList.Nil = pure ()

instance (
	AllocationCallbacks.ToMiddle mac,
	DestroyShaderModuleList foos macs ) =>
	DestroyShaderModuleList (foo ': foos) (mac ': macs) where
	destroyShaderModuleList dvc (U3 cim :** cims) (mac :** macs) =
		destroyShaderModule dvc cim mac >>
		destroyShaderModuleList dvc cims macs
