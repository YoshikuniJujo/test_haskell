{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Graphics.Middle.Internal (
	CreateInfo(..), CreateInfoListToCore,
	createGs, recreateGs,

	destroyGs,

	G, gNull, gToCore,
	) where

import Prelude hiding (length)
import Prelude qualified as P

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable.PeekPoke
import Control.Monad
import Data.TypeLevel.Uncurry
import Data.TypeLevel.Length
import Data.IORef
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:**))
import Data.Word
import Data.Int

import Gpu.Vulkan.Misc.Middle.Internal
import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Pipeline.Enum

import qualified Gpu.Vulkan.Pipeline.Core as Pipeline.C
import qualified Gpu.Vulkan.Pipeline.ShaderStage.Middle.Internal as ShaderStage
import qualified Gpu.Vulkan.Pipeline.VertexInputState.Middle.Internal as VertexInputState.M
import Gpu.Vulkan.Pipeline.InputAssemblyState.Middle.Internal
	qualified as InputAssemblyState
import Gpu.Vulkan.Pipeline.TessellationState.Middle.Internal
	qualified as TessellationState
import qualified Gpu.Vulkan.Pipeline.ViewportState.Middle.Internal as ViewportState
import qualified Gpu.Vulkan.Pipeline.RasterizationState.Middle.Internal as RasterizationState
import qualified Gpu.Vulkan.Pipeline.MultisampleState.Middle.Internal as MultisampleState
import qualified Gpu.Vulkan.Pipeline.DepthStencilState.Middle.Internal
	as DepthStencilState
import qualified Gpu.Vulkan.Pipeline.ColorBlendState.Middle.Internal
	as ColorBlendState
import qualified Gpu.Vulkan.Pipeline.DynamicState.Middle.Internal
	as DynamicState
import qualified Gpu.Vulkan.Pipeline.Layout.Middle.Internal as Layout
import qualified Gpu.Vulkan.RenderPass.Middle.Internal as RenderPass
import qualified Gpu.Vulkan.Pipeline.Graphics.Core as C

import Gpu.Vulkan.AllocationCallbacks.Middle.Internal
	qualified as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Middle.Internal as Device
import qualified Gpu.Vulkan.Pipeline.Cache.Middle.Internal as Cache

data CreateInfo n nskndvss vis ias ts vs rs ms dss cbs ds = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoStages :: HeteroParList.PL (U3 ShaderStage.CreateInfo) nskndvss,
	createInfoVertexInputState :: Maybe (VertexInputState.M.CreateInfo vis),
	createInfoInputAssemblyState ::
		Maybe (InputAssemblyState.CreateInfo ias),
	createInfoTessellationState :: Maybe (TessellationState.CreateInfo ts),
	createInfoViewportState :: Maybe (ViewportState.CreateInfo vs),
	createInfoRasterizationState ::
		Maybe (RasterizationState.CreateInfo rs),
	createInfoMultisampleState :: Maybe (MultisampleState.CreateInfo ms),
	createInfoDepthStencilState :: Maybe (DepthStencilState.CreateInfo dss),
	createInfoColorBlendState :: Maybe (ColorBlendState.CreateInfo cbs),
	createInfoDynamicState :: Maybe (DynamicState.CreateInfo ds),
	createInfoLayout :: Layout.L,
	createInfoRenderPass :: RenderPass.R,
	createInfoSubpass :: Word32,
	createInfoBasePipelineHandle :: G,
	createInfoBasePipelineIndex :: Int32 }

createInfoToCore :: (
	WithPoked n,
	ShaderStage.CreateInfoListToCore nskndvss,
	WithPoked n2, WithPoked n3, WithPoked n4,
	WithPoked n5, WithPoked n6, WithPoked n7, WithPoked n8, WithPoked n9,
	WithPoked n10 ) =>
	CreateInfo n nskndvss n2 n3 n4 n5 n6 n7 n8 n9 n10 ->
	(C.CreateInfo -> IO a) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoStages = ss,
	createInfoVertexInputState = mvist,
	createInfoInputAssemblyState = miast,
	createInfoTessellationState = mtst,
	createInfoViewportState = mvst,
	createInfoRasterizationState = mrst,
	createInfoMultisampleState = mmst,
	createInfoDepthStencilState = mdsst,
	createInfoColorBlendState = mcbst,
	createInfoDynamicState = mdst,
	createInfoLayout = Layout.L lyt,
	createInfoRenderPass = RenderPass.R rp,
	createInfoSubpass = sp,
	createInfoBasePipelineHandle = bph,
	createInfoBasePipelineIndex = bpi } f =
	withPokedMaybe' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	ShaderStage.createInfoListToCore ss \css ->
	let sc = P.length css in
	allocaArray sc \pss ->
	pokeArray pss css >>
	maybeToCore VertexInputState.M.createInfoToCore mvist \pvist ->
	maybeToCore InputAssemblyState.createInfoToCore miast \piast ->
	maybeToCore TessellationState.createInfoToCore mtst \ptst ->
	maybeToCore ViewportState.createInfoToCore mvst \pvst ->
	maybeToCore RasterizationState.createInfoToCore mrst \prst ->
	maybeToCore MultisampleState.createInfoToCore mmst \pmst ->
	maybeToCore DepthStencilState.createInfoToCore mdsst \pdsst ->
	maybeToCore ColorBlendState.createInfoToCore mcbst \pcbst ->
	maybeToCore' DynamicState.createInfoToCore mdst \pdst ->
	gToCore bph >>= \bph' ->
	f C.CreateInfo {
		C.createInfoSType = (),
		C.createInfoPNext = pnxt',
		C.createInfoFlags = flgs,
		C.createInfoStageCount = fromIntegral sc,
		C.createInfoPStages = pss,
		C.createInfoPVertexInputState = pvist,
		C.createInfoPInputAssemblyState = piast,
		C.createInfoPTessellationState = ptst,
		C.createInfoPViewportState = pvst,
		C.createInfoPRasterizationState = prst,
		C.createInfoPMultisampleState = pmst,
		C.createInfoPDepthStencilState = pdsst,
		C.createInfoPColorBlendState = pcbst,
		C.createInfoPDynamicState = pdst,
		C.createInfoLayout = lyt,
		C.createInfoRenderPass = rp,
		C.createInfoSubpass = sp,
		C.createInfoBasePipelineHandle = bph',
		C.createInfoBasePipelineIndex = bpi }

maybeToCore :: (a -> (Ptr b -> IO r) -> IO r) -> Maybe a -> (Ptr b -> IO r) -> IO r
maybeToCore f mx g = case mx of Nothing -> g NullPtr; Just x -> f x g

maybeToCore' :: (a -> (Ptr b -> IO r) -> IO ()) -> Maybe a -> (Ptr b -> IO r) -> IO ()
maybeToCore' f mx g = case mx of Nothing -> () <$ g NullPtr; Just x -> f x g

class Length ass => CreateInfoListToCore ass where
	createInfoListToCore ::
		HeteroParList.PL (U11 CreateInfo) ass ->
		([C.CreateInfo] -> IO r) -> IO ()

instance CreateInfoListToCore '[] where createInfoListToCore HeteroParList.Nil f = () <$ f []

instance (
	WithPoked n, ShaderStage.CreateInfoListToCore nskndvss,
	WithPoked vis, WithPoked ias, WithPoked ts, WithPoked vs,
	WithPoked rs, WithPoked ms, WithPoked dss, WithPoked cbs, WithPoked ds,
	CreateInfoListToCore ass
	) =>
	CreateInfoListToCore ('(
		n, nskndvss, vis, ias, ts, vs, rs, ms, dss, cbs, ds ) ': ass) where
	createInfoListToCore (U11 ci :** cis) f =
		createInfoToCore ci \cci ->
		createInfoListToCore cis \ccis -> f $ cci : ccis

gNull :: IO G
gNull = G <$> newIORef NullHandle

newtype G = G (IORef Pipeline.C.P)

gToCore :: G -> IO Pipeline.C.P
gToCore (G rp) = readIORef rp

gFromCore :: Pipeline.C.P -> IO G
gFromCore p = G <$> newIORef p

gListFromCore :: [Pipeline.C.P] -> IO [G]
gListFromCore [] = pure []
gListFromCore (cp : cps) = (:) <$> gFromCore cp <*> gListFromCore cps

gListToIORefs :: [G] -> [IORef Pipeline.C.P]
gListToIORefs [] = []
gListToIORefs (G cp : cps) = cp : gListToIORefs cps

gListToCore :: [G] -> IO [Pipeline.C.P]
gListToCore cps = readIORef `mapM` gListToIORefs cps

createGs :: (CreateInfoListToCore as, WithPoked c) =>
	Device.D -> Maybe Cache.C -> HeteroParList.PL (U11 CreateInfo) as ->
	Maybe (AllocationCallbacks.A c) -> IO [G]
createGs dvc mc cis mac = gListFromCore =<< createRaw dvc mc cis mac

recreateGs :: (
	CreateInfoListToCore as, WithPoked c, WithPoked d
	) =>
	Device.D -> Maybe Cache.C ->
	HeteroParList.PL (U11 CreateInfo) as ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	[G] -> IO ()
recreateGs dvc mc cis macc macd gs =
	recreateRaw dvc mc cis macc macd $ gListToIORefs gs

createRaw :: forall ss n' . (CreateInfoListToCore ss, WithPoked n') =>
	Device.D -> Maybe Cache.C ->
	HeteroParList.PL (U11 CreateInfo) ss ->
	Maybe (AllocationCallbacks.A n') -> IO [Pipeline.C.P]
createRaw (Device.D dvc) mc cis mac = let
	cc = case mc of Nothing -> NullPtr; Just (Cache.C c) -> c
	cic = length @_ @ss in
	allocaArray cic \pps -> do
		createInfoListToCore cis \ccis -> allocaArray cic \pcis ->
			pokeArray pcis ccis >>
			AllocationCallbacks.maybeToCore' mac \pac -> do
				r <- C.create dvc cc (fromIntegral cic) pcis pac pps
				throwUnlessSuccess $ Result r
		peekArray cic pps

recreateRaw :: (CreateInfoListToCore ss, WithPoked c, WithPoked d) =>
	Device.D -> Maybe Cache.C ->
	HeteroParList.PL (U11 CreateInfo) ss ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	[IORef Pipeline.C.P] -> IO ()
recreateRaw dvc mc cis macc macd rs = do
	os <- readIORef `mapM` rs
	ns <- createRaw dvc mc cis macc
	zipWithM_ writeIORef rs ns
	(\o -> destroyRaw dvc o macd) `mapM_` os

destroyGs :: WithPoked d =>
	Device.D -> [G] -> Maybe (AllocationCallbacks.A d) -> IO ()
destroyGs dvc gs mac = ((\g -> gFromCore g >>= \g' -> destroy dvc g' mac) `mapM_`) =<< gListToCore gs

destroy :: WithPoked n =>
	Device.D -> G -> Maybe (AllocationCallbacks.A n) -> IO ()
destroy (Device.D dvc) g mac = gToCore g >>= \p ->
	AllocationCallbacks.maybeToCore' mac $ Pipeline.C.destroy dvc p

destroyRaw :: WithPoked d =>
	Device.D -> Pipeline.C.P -> Maybe (AllocationCallbacks.A d) -> IO ()
destroyRaw (Device.D dvc) p macd =
	AllocationCallbacks.maybeToCore' macd $ Pipeline.C.destroy dvc p
