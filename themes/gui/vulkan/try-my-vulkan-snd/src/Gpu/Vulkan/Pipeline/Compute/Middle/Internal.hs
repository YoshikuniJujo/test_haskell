{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Compute.Middle.Internal (
	C(..),

	createCsNew, destroy, CreateInfoNew(..), CreateInfoListToCoreNew ) where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable.PeekPoke
import Foreign.Pointable hiding (NullPtr)
import Control.Monad.Cont
import Data.HeteroList
import Data.Maybe
import Data.Int

import Shaderc.EnumAuto

import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum

import Gpu.Vulkan.AllocationCallbacks.Middle.Internal
	qualified as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Middle.Internal as Device
import qualified Gpu.Vulkan.Pipeline.Enum as Pipeline
import qualified Gpu.Vulkan.Pipeline.Core as Pipeline.C
import qualified Gpu.Vulkan.Pipeline.Cache.Middle.Internal as Cache
import qualified Gpu.Vulkan.Pipeline.Compute.Core as C
import qualified Gpu.Vulkan.Pipeline.ShaderStage.Middle.Internal as ShaderStage
import qualified Gpu.Vulkan.Pipeline.Layout.Middle.Internal as Pipeline.Layout

data CreateInfoNew n ns vs = CreateInfoNew {
	createInfoNextNew :: Maybe n,
	createInfoFlagsNew :: Pipeline.CreateFlags,
	createInfoStageNew :: ShaderStage.CreateInfoNew ns 'GlslComputeShader vs,
	createInfoLayoutNew :: Pipeline.Layout.L,
	createInfoBasePipelineHandleNew :: Maybe C,
	createInfoBasePipelineIndexNew :: Maybe Int32 }

deriving instance (
	Show n, Show (ShaderStage.CreateInfoNew ns 'GlslComputeShader vs) ) =>
	Show (CreateInfoNew n ns vs)

createInfoToCoreNew ::
	(Pointable n, Pointable n1, SizableList vs, StoreHetero' vs) =>
	CreateInfoNew n n1 vs -> ContT r IO C.CreateInfo
createInfoToCoreNew CreateInfoNew {
	createInfoNextNew = mnxt,
	createInfoFlagsNew = Pipeline.CreateFlagBits flgs,
	createInfoStageNew = stg,
	createInfoLayoutNew = Pipeline.Layout.L lyt,
	createInfoBasePipelineHandleNew = maybe NullPtr (\(C b) -> b) -> bph,
	createInfoBasePipelineIndexNew = fromMaybe (- 1) -> idx
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	stg' <- ShaderStage.createInfoToCoreNew stg
	pure C.CreateInfo {
		C.createInfoSType = (),
		C.createInfoPNext = pnxt,
		C.createInfoFlags = flgs,
		C.createInfoStage = stg',
		C.createInfoLayout = lyt,
		C.createInfoBasePipelineHandle = bph,
		C.createInfoBasePipelineIndex = idx }

class CreateInfoListToCoreNew vss where
	createInfoListToCoreNew ::
		HeteroVarList (V3 CreateInfoNew) vss -> ContT r IO [C.CreateInfo]

instance CreateInfoListToCoreNew '[] where createInfoListToCoreNew HVNil = pure []

instance (
	Pointable n, Pointable n1, SizableList vss, StoreHetero' vss,
	CreateInfoListToCoreNew as ) =>
	CreateInfoListToCoreNew ('(n, n1, vss) ': as) where
	createInfoListToCoreNew (V3 ci :...: cis) = (:)
		<$> createInfoToCoreNew ci
		<*> createInfoListToCoreNew cis

newtype C = C Pipeline.C.P deriving Show

createCsNew :: (CreateInfoListToCoreNew vss, Pokable c) =>
	Device.D -> Maybe Cache.C -> HeteroVarList (V3 CreateInfoNew) vss ->
	Maybe (AllocationCallbacks.A c) -> IO [C]
createCsNew (Device.D dvc) (maybe NullPtr (\(Cache.C c) -> c) -> cch) cis mac =
	((C <$>) <$>) . ($ pure) $ runContT do
		cis' <- createInfoListToCoreNew cis
		let	ln = length cis'
		pcis <- ContT $ allocaArray ln
		lift $ pokeArray pcis cis'
		pac <- AllocationCallbacks.maybeToCore mac
		pps <- ContT $ allocaArray ln
		lift do	r <- C.createCs dvc cch (fromIntegral ln) pcis pac pps
			throwUnlessSuccess $ Result r
			peekArray ln pps

destroy :: Pokable d =>
		Device.D -> C -> Maybe (AllocationCallbacks.A d) -> IO ()
destroy (Device.D dvc) (C p) mac = ($ pure) $ runContT do
	pac <- AllocationCallbacks.maybeToCore mac
	lift $ Pipeline.C.destroy dvc p pac
