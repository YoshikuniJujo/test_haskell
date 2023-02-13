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

	createCs, destroy, CreateInfo(..), CreateInfoListToCore ) where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable.PeekPoke
import Foreign.Storable.Hetero
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

data CreateInfo n ns vs = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: Pipeline.CreateFlags,
	createInfoStage :: ShaderStage.CreateInfo ns 'GlslComputeShader vs,
	createInfoLayout :: Pipeline.Layout.L,
	createInfoBasePipelineHandle :: Maybe C,
	createInfoBasePipelineIndex :: Maybe Int32 }

deriving instance (
	Show n, Show (ShaderStage.CreateInfo ns 'GlslComputeShader vs) ) =>
	Show (CreateInfo n ns vs)

createInfoToCore ::
	(Pokable n, Pokable n1, PokableList vs) =>
	CreateInfo n n1 vs -> ContT r IO C.CreateInfo
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = Pipeline.CreateFlagBits flgs,
	createInfoStage = stg,
	createInfoLayout = Pipeline.Layout.L lyt,
	createInfoBasePipelineHandle = maybe NullPtr (\(C b) -> b) -> bph,
	createInfoBasePipelineIndex = fromMaybe (- 1) -> idx
	} = do
	(castPtr -> pnxt) <- ContT $ withPokedMaybe mnxt
	stg' <- ContT $ ShaderStage.createInfoToCore stg
	pure C.CreateInfo {
		C.createInfoSType = (),
		C.createInfoPNext = pnxt,
		C.createInfoFlags = flgs,
		C.createInfoStage = stg',
		C.createInfoLayout = lyt,
		C.createInfoBasePipelineHandle = bph,
		C.createInfoBasePipelineIndex = idx }

class CreateInfoListToCore vss where
	createInfoListToCore ::
		HeteroVarList (V3 CreateInfo) vss -> ContT r IO [C.CreateInfo]

instance CreateInfoListToCore '[] where createInfoListToCore HVNil = pure []

instance (
	Pokable n, Pokable n1, PokableList vss,
	CreateInfoListToCore as ) =>
	CreateInfoListToCore ('(n, n1, vss) ': as) where
	createInfoListToCore (V3 ci :...: cis) = (:)
		<$> createInfoToCore ci
		<*> createInfoListToCore cis

newtype C = C Pipeline.C.P deriving Show

createCs :: (CreateInfoListToCore vss, WithPoked c) =>
	Device.D -> Maybe Cache.C -> HeteroVarList (V3 CreateInfo) vss ->
	Maybe (AllocationCallbacks.A c) -> IO [C]
createCs (Device.D dvc) (maybe NullPtr (\(Cache.C c) -> c) -> cch) cis mac =
	((C <$>) <$>) . ($ pure) $ runContT do
		cis' <- createInfoListToCore cis
		let	ln = length cis'
		pcis <- ContT $ allocaArray ln
		lift $ pokeArray pcis cis'
		ContT \f -> allocaArray ln \pps -> do
			AllocationCallbacks.maybeToCore' mac \pac -> do
				r <- C.createCs dvc cch (fromIntegral ln) pcis pac pps
				throwUnlessSuccess $ Result r
			f =<< peekArray ln pps

destroy :: WithPoked d =>
	Device.D -> C -> Maybe (AllocationCallbacks.A d) -> IO ()
destroy (Device.D dvc) (C p) mac =
	AllocationCallbacks.maybeToCore' mac $ Pipeline.C.destroy dvc p
