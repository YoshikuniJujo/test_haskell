{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.ShaderStage.Middle.Internal (
	CreateInfo(..), createInfoToCore,
	CreateInfoListToCore, createInfoListToCore,

	CreateInfoNew(..), createInfoToCoreNew,
	CreateInfoListToCoreNew, createInfoListToCoreNew ) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Pointable
import Control.Monad.Cont
import Data.HeteroList

import qualified Data.ByteString as BS

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Pipeline.ShaderStage.Enum

import qualified Gpu.Vulkan.ShaderModule.Middle.Internal as ShaderModule
import qualified Gpu.Vulkan.Pipeline.ShaderStage.Core as C
import qualified Gpu.Vulkan.Specialization.Middle.Internal as Specialization
import qualified Gpu.Vulkan.Specialization.Core as Specialization.C

data CreateInfo n sknd vs = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoStage :: ShaderStageFlagBits,
	createInfoModule :: ShaderModule.M sknd,
	createInfoName :: BS.ByteString,
	createInfoSpecializationInfo :: Maybe vs }
	deriving Show

data CreateInfoNew n sknd vs = CreateInfoNew {
	createInfoNextNew :: Maybe n,
	createInfoFlagsNew :: CreateFlags,
	createInfoStageNew :: ShaderStageFlagBits,
	createInfoModuleNew :: ShaderModule.M sknd,
	createInfoNameNew :: BS.ByteString,
	createInfoSpecializationInfoNew :: Maybe (HeteroList' vs) }

deriving instance (Show n, Show (HeteroList' vs)) => Show (CreateInfoNew n sknd vs)

createInfoToCore ::
	forall n sknd vs r . (Pointable n, Specialization.StoreValues vs) =>
	CreateInfo n sknd vs -> ContT r IO C.CreateInfo
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoStage = ShaderStageFlagBits stg,
	createInfoModule = ShaderModule.M mdl,
	createInfoName = nm,
	createInfoSpecializationInfo = mxs } = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	cnm <- ContT $ BS.useAsCString nm
	pcsi <- case mxs of
		Nothing -> pure NullPtr
		Just xs -> do
			Specialization.C.Info_ fcsi <- Specialization.infoToCore xs
			ContT $ withForeignPtr fcsi
	pure C.CreateInfo {
		C.createInfoSType = (),
		C.createInfoPNext = pnxt,
		C.createInfoFlags = flgs,
		C.createInfoStage = stg,
		C.createInfoModule = mdl,
		C.createInfoPName = cnm,
		C.createInfoPSpecializationInfo = pcsi }

createInfoToCoreNew ::
	forall n sknd vs r . (Pointable n, StorableList' vs, StoreHetero' vs) =>
	CreateInfoNew n sknd vs -> ContT r IO C.CreateInfo
createInfoToCoreNew CreateInfoNew {
	createInfoNextNew = mnxt,
	createInfoFlagsNew = CreateFlagBits flgs,
	createInfoStageNew = ShaderStageFlagBits stg,
	createInfoModuleNew = ShaderModule.M mdl,
	createInfoNameNew = nm,
	createInfoSpecializationInfoNew = mxs } = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	cnm <- ContT $ BS.useAsCString nm
	pcsi <- case mxs of
		Nothing -> pure NullPtr
		Just xs -> do
			Specialization.C.Info_ fcsi <- Specialization.infoToCore' xs
			ContT $ withForeignPtr fcsi
	pure C.CreateInfo {
		C.createInfoSType = (),
		C.createInfoPNext = pnxt,
		C.createInfoFlags = flgs,
		C.createInfoStage = stg,
		C.createInfoModule = mdl,
		C.createInfoPName = cnm,
		C.createInfoPSpecializationInfo = pcsi }

class CreateInfoListToCore sss where
	createInfoListToCore ::
		HeteroVarList (V3 CreateInfo) sss -> ContT r IO [C.CreateInfo]

instance CreateInfoListToCore '[] where createInfoListToCore HVNil = pure []

instance (
	Pointable n, Specialization.StoreValues vs, CreateInfoListToCore sss
	) => CreateInfoListToCore ('(n, sknd, vs) ': sss) where
	createInfoListToCore (V3 ci :...: cis) = (:)
		<$> createInfoToCore ci
		<*> createInfoListToCore cis

class CreateInfoListToCoreNew sss where
	createInfoListToCoreNew ::
		HeteroVarList (V3 CreateInfoNew) sss -> ContT r IO [C.CreateInfo]

instance CreateInfoListToCoreNew '[] where createInfoListToCoreNew HVNil = pure []

instance (
	Pointable n, StorableList' vs, StoreHetero' vs, CreateInfoListToCoreNew sss
	) => CreateInfoListToCoreNew ('(n, sknd, vs) ': sss) where
	createInfoListToCoreNew (V3 ci :...: cis) = (:)
		<$> createInfoToCoreNew ci
		<*> createInfoListToCoreNew cis
