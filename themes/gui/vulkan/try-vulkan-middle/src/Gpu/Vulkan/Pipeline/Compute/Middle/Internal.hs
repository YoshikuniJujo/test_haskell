{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Compute.Middle.Internal (
	C(..),

	createCs, destroy, CreateInfo(..), CreateInfoListToCore ) where

import Prelude hiding (length)

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable.PeekPoke
import Foreign.Storable.HeteroList
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.List
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:**))
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
import qualified Gpu.Vulkan.PipelineCache.Middle.Internal as Cache
import qualified Gpu.Vulkan.Pipeline.Compute.Core as C
import qualified Gpu.Vulkan.Pipeline.ShaderStage.Middle.Internal as ShaderStage
import qualified Gpu.Vulkan.PipelineLayout.Middle.Internal as Pipeline.Layout

data CreateInfo mn ss sivs = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: Pipeline.CreateFlags,
	createInfoStage :: ShaderStage.CreateInfo ss 'GlslComputeShader sivs,
	createInfoLayout :: Pipeline.Layout.P,
	createInfoBasePipelineHandle :: Maybe C,
	createInfoBasePipelineIndex :: Maybe Int32 }

deriving instance (
	Show (TMaybe.M mn), Show (ShaderStage.CreateInfo ss 'GlslComputeShader sivs) ) =>
	Show (CreateInfo mn ss sivs)

createInfoToCore ::
	(WithPoked (TMaybe.M mn), WithPoked (TMaybe.M n1), PokableList vs) =>
	CreateInfo mn n1 vs -> (C.CreateInfo -> IO r) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = Pipeline.CreateFlagBits flgs,
	createInfoStage = stg,
	createInfoLayout = Pipeline.Layout.P lyt,
	createInfoBasePipelineHandle = maybe NullPtr (\(C b) -> b) -> bph,
	createInfoBasePipelineIndex = fromMaybe (- 1) -> idx } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	ShaderStage.createInfoToCore stg \stg' ->
	f C.CreateInfo {
		C.createInfoSType = (),
		C.createInfoPNext = pnxt',
		C.createInfoFlags = flgs,
		C.createInfoStage = stg',
		C.createInfoLayout = lyt,
		C.createInfoBasePipelineHandle = bph,
		C.createInfoBasePipelineIndex = idx }

class Length cias => CreateInfoListToCore cias where
	createInfoListToCore ::
		HeteroParList.PL (U3 CreateInfo) cias ->
		([C.CreateInfo] -> IO r) -> IO ()

instance CreateInfoListToCore '[] where
	createInfoListToCore HeteroParList.Nil = (() <$) . ($ [])

instance (
	WithPoked (TMaybe.M mn), WithPoked (TMaybe.M ss), PokableList sivs,
	CreateInfoListToCore cias ) =>
	CreateInfoListToCore ('(mn, ss, sivs) ': cias) where
	createInfoListToCore (U3 ci :** cis) f =
		createInfoToCore ci \cci ->
		createInfoListToCore cis \ccis -> f $ cci : ccis

newtype C = C Pipeline.C.P deriving Show

createCs :: forall cias mc . CreateInfoListToCore cias =>
	Device.D -> Maybe Cache.P -> HeteroParList.PL (U3 CreateInfo) cias ->
	TPMaybe.M AllocationCallbacks.A mc -> IO [C]
createCs (Device.D dvc) (maybe NullPtr (\(Cache.P c) -> c) -> cch) cis mac =
	(C <$>) <$> allocaArray ln \pps -> do
		createInfoListToCore cis \cis' ->
			allocaArray ln \pcis ->
			pokeArray pcis cis' >>
			AllocationCallbacks.mToCore mac \pac ->
				throwUnlessSuccess . Result =<< C.createCs
					dvc cch (fromIntegral ln) pcis pac pps
		peekArray ln pps
	where ln = length @_ @cias

destroy :: Device.D -> C -> TPMaybe.M AllocationCallbacks.A md -> IO ()
destroy (Device.D dvc) (C p) mac =
	AllocationCallbacks.mToCore mac $ Pipeline.C.destroy dvc p
