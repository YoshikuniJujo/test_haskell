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
import Data.TypeLevel.Uncurry
import Data.TypeLevel.Length
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:*), pattern (:**))
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

data CreateInfo n ss sivs = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: Pipeline.CreateFlags,
	createInfoStage :: ShaderStage.CreateInfo ss 'GlslComputeShader sivs,
	createInfoLayout :: Pipeline.Layout.L,
	createInfoBasePipelineHandle :: Maybe C,
	createInfoBasePipelineIndex :: Maybe Int32 }

deriving instance (
	Show n, Show (ShaderStage.CreateInfo ss 'GlslComputeShader sivs) ) =>
	Show (CreateInfo n ss sivs)

createInfoToCore ::
	(WithPoked n, WithPoked n1, PokableList vs) =>
	CreateInfo n n1 vs -> (C.CreateInfo -> IO r) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = Pipeline.CreateFlagBits flgs,
	createInfoStage = stg,
	createInfoLayout = Pipeline.Layout.L lyt,
	createInfoBasePipelineHandle = maybe NullPtr (\(C b) -> b) -> bph,
	createInfoBasePipelineIndex = fromMaybe (- 1) -> idx } f =
	withPokedMaybe' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
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
	WithPoked n, WithPoked ss, PokableList sivs,
	CreateInfoListToCore cias ) =>
	CreateInfoListToCore ('(n, ss, sivs) ': cias) where
	createInfoListToCore (U3 ci :** cis) f =
		createInfoToCore ci \cci ->
		createInfoListToCore cis \ccis -> f $ cci : ccis

newtype C = C Pipeline.C.P deriving Show

createCs :: forall cias c .
	(CreateInfoListToCore cias, WithPoked c) =>
	Device.D -> Maybe Cache.C -> HeteroParList.PL (U3 CreateInfo) cias ->
	Maybe (AllocationCallbacks.A c) -> IO [C]
createCs (Device.D dvc) (maybe NullPtr (\(Cache.C c) -> c) -> cch) cis mac =
	(C <$>) <$> allocaArray ln \pps -> do
		createInfoListToCore cis \cis' ->
			allocaArray ln \pcis ->
			pokeArray pcis cis' >>
			AllocationCallbacks.maybeToCore mac \pac ->
				throwUnlessSuccess . Result =<< C.createCs
					dvc cch (fromIntegral ln) pcis pac pps
		peekArray ln pps
	where ln = length @_ @cias

destroy :: WithPoked d =>
	Device.D -> C -> Maybe (AllocationCallbacks.A d) -> IO ()
destroy (Device.D dvc) (C p) mac =
	AllocationCallbacks.maybeToCore mac $ Pipeline.C.destroy dvc p
