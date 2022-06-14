{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.Compute.Middle where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Pointable
import Control.Monad.Cont
import Data.HeteroList hiding (length)
import Data.Maybe
import Data.Int

import Shaderc.EnumAuto

import Vulkan.Exception
import Vulkan.Exception.Enum

import qualified Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Vulkan.Device.Middle as Device
import qualified Vulkan.Pipeline.Enum as Pipeline
import qualified Vulkan.Pipeline.Core as Pipeline.C
import qualified Vulkan.Pipeline.Cache.Middle as Cache
import qualified Vulkan.Pipeline.Compute.Core as C
import qualified Vulkan.Pipeline.ShaderStage.Middle as ShaderStage
import qualified Vulkan.Pipeline.Layout.Middle as Pipeline.Layout
import qualified Vulkan.Specialization as Specialization

data CreateInfo n n1 vs = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: Pipeline.CreateFlags,
	createInfoStage :: ShaderStage.CreateInfo n1 'GlslComputeShader vs,
	createInfoLayout :: Pipeline.Layout.L,
	createInfoBasePipelineHandle :: Maybe C,
	createInfoBasePipelineIndex :: Maybe Int32 }
	deriving Show

createInfoToCore ::
	(Pointable n, Pointable n1, Specialization.StoreValues vs) =>
	CreateInfo n n1 vs -> ContT r IO C.CreateInfo
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = Pipeline.CreateFlagBits flgs,
	createInfoStage = stg,
	createInfoLayout = Pipeline.Layout.L lyt,
	createInfoBasePipelineHandle = maybe NullPtr (\(C b) -> b) -> bph,
	createInfoBasePipelineIndex = fromMaybe (- 1) -> idx
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	stg' <- ShaderStage.createInfoToCore stg
	pure C.CreateInfo {
		C.createInfoSType = (),
		C.createInfoPNext = pnxt,
		C.createInfoFlags = flgs,
		C.createInfoStage = stg',
		C.createInfoLayout = lyt,
		C.createInfoBasePipelineHandle = bph,
		C.createInfoBasePipelineIndex = idx }

class CreateInfosToCore vss where
	createInfosToCore :: (Pointable n, Pointable n1) =>
		HeteroVarList (CreateInfo n n1) vss -> ContT r IO [C.CreateInfo]

instance CreateInfosToCore '[] where createInfosToCore HVNil = pure []

instance (Specialization.StoreValues a, CreateInfosToCore as) =>
	CreateInfosToCore (a ': as) where
	createInfosToCore (ci :...: cis) = do
		cci <- createInfoToCore ci
		ccis <- createInfosToCore cis
		pure $ cci : ccis

newtype C = C Pipeline.C.P deriving Show

createCs :: (Pointable n, Pointable n1, CreateInfosToCore vss, Pointable c) =>
	Device.D -> Cache.C -> HeteroVarList (CreateInfo n n1) vss ->
	Maybe (AllocationCallbacks.A c) -> IO [C]
createCs (Device.D dvc) (Cache.C c) cis mac =
	((C <$>) <$>) . ($ pure) $ runContT do
		cis' <- createInfosToCore cis
		let	ln = length cis'
		pcis <- ContT $ allocaArray ln
		lift $ pokeArray pcis cis'
		pac <- AllocationCallbacks.maybeToCore mac
		pps <- ContT $ allocaArray ln
		lift do	r <- C.createCs dvc c (fromIntegral ln) pcis pac pps
			throwUnlessSuccess $ Result r
			peekArray ln pps

destroy :: Pointable d =>
	Device.D -> C -> Maybe (AllocationCallbacks.A d) -> IO ()
destroy (Device.D dvc) (C p) mac = ($ pure) $ runContT do
	pac <- AllocationCallbacks.maybeToCore mac
	lift $ Pipeline.C.destroy dvc p pac
