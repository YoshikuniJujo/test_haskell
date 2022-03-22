{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.ShaderStage where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Pointable
import Control.Monad.Cont

import qualified Data.ByteString as BS

import Vulkan.Pipeline.ShaderStage.Enum

import qualified Vulkan.Shader.Stage.Enum as ShaderStage
import qualified Vulkan.Shader.Module as ShaderModule
import qualified Vulkan.Pipeline.ShaderStage.Core as C
import qualified Vulkan.Specialization as Specialization
import qualified Vulkan.Specialization.Core as Specialization.C

data CreateInfo n sknd vs = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoStage :: ShaderStage.FlagBits,
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
	createInfoStage = ShaderStage.FlagBits stg,
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
