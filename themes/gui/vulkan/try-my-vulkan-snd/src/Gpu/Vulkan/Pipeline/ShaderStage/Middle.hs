{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.ShaderStage.Middle where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Pointable
import Control.Monad.Cont

import qualified Data.ByteString as BS

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Pipeline.ShaderStage.Enum

import qualified Gpu.Vulkan.ShaderModule.Middle as ShaderModule
import qualified Gpu.Vulkan.Pipeline.ShaderStage.Core as C
import qualified Gpu.Vulkan.Specialization as Specialization
import qualified Gpu.Vulkan.Specialization.Core as Specialization.C

data CreateInfo n sknd vs = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoStage :: ShaderStageFlagBits,
	createInfoModule :: ShaderModule.M sknd,
	createInfoName :: BS.ByteString,
	createInfoSpecializationInfo :: Maybe vs }
	deriving Show

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

infixr 5 `CreateInfoCons`

data CreateInfoList n sknds vss where
	CreateInfoNil :: CreateInfoList n '[] '[]
	CreateInfoCons :: CreateInfo n sknd vs -> CreateInfoList n sknds vss ->
		CreateInfoList n (sknd ': sknds) (vs ': vss)

class CreateInfoListToCore n sknds vss where
	createInfoListToCore :: CreateInfoList n sknds vss ->
		ContT r IO [C.CreateInfo]

deriving instance (Show n, Show vs, Show (CreateInfoList n sknds vss)) =>
	Show (CreateInfoList n (sknd ': sknds) (vs ': vss))

instance CreateInfoListToCore n '[] '[] where createInfoListToCore _ = pure []

instance (
	Pointable n, Specialization.StoreValues vs,
	CreateInfoListToCore n sknds vss ) =>
	CreateInfoListToCore n (sknd ': sknds) (vs ': vss) where
	createInfoListToCore (ci `CreateInfoCons` cis) = (:)
		<$> createInfoToCore ci
		<*> createInfoListToCore cis
