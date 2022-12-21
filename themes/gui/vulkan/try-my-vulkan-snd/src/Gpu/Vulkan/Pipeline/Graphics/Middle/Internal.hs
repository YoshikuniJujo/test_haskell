{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Graphics.Middle.Internal (
	G, gNull, GListFromCore, GListVars,
	CreateInfo'(..), CreateInfoListToCore',
	createGs', recreateGs', destroyGs',

	gToCore
	) where

import GHC.TypeNats
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Pointable
import Control.Monad.Cont
import Data.Kind
import Data.IORef
import Data.HeteroList hiding (length)
import Data.Word
import Data.Int

import Shaderc.EnumAuto

import Gpu.Vulkan.Base
import Gpu.Vulkan.Exception
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Pipeline.Enum

import qualified Gpu.Vulkan.Pipeline.Core as Pipeline.C
import qualified Gpu.Vulkan.Pipeline.ShaderStage.Middle.Internal as ShaderStage
import qualified Gpu.Vulkan.Pipeline.VertexInputState as VertexInputState
import Gpu.Vulkan.Pipeline.InputAssemblyState.Middle.Internal
	qualified as InputAssemblyState
import qualified Gpu.Vulkan.Pipeline.TessellationState as TessellationState
import qualified Gpu.Vulkan.Pipeline.ViewportState as ViewportState
import qualified Gpu.Vulkan.Pipeline.RasterizationState.Middle.Internal as RasterizationState
import qualified Gpu.Vulkan.Pipeline.MultisampleState.Middle.Internal as MultisampleState
import qualified Gpu.Vulkan.Pipeline.DepthStencilState.Middle.Internal
	as DepthStencilState
import qualified Gpu.Vulkan.Pipeline.ColorBlendState.Middle.Internal
	as ColorBlendState
import qualified Gpu.Vulkan.Pipeline.DynamicState.Middle.Internal
	as DynamicState
import qualified Gpu.Vulkan.Pipeline.Layout.Middle.Internal as Layout
import qualified Gpu.Vulkan.RenderPass.Middle as RenderPass
import qualified Gpu.Vulkan.Pipeline.Graphics.Core as C
import qualified Gpu.Vulkan.Pipeline.VertexInputState.BindingStrideList
	as BindingStrideList
import qualified Gpu.Vulkan.VertexInput as VertexInput

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Middle.Internal as Device
import qualified Gpu.Vulkan.Pipeline.Cache.Middle.Internal as Cache

data CreateInfo' n nskndvss nvsts n3 n4 n5 n6 n7 n8 n9 n10 vsts' = CreateInfo' {
	createInfoNext' :: Maybe n,
	createInfoFlags' :: CreateFlags,
	createInfoStages' :: HeteroVarList (V3 ShaderStage.CreateInfo) nskndvss,
	createInfoVertexInputState' ::
		Maybe (V3 VertexInputState.CreateInfo nvsts),
	createInfoInputAssemblyState' ::
		Maybe (InputAssemblyState.CreateInfo n3),
	createInfoTessellationState' :: Maybe (TessellationState.CreateInfo n4),
	createInfoViewportState' :: Maybe (ViewportState.CreateInfo n5),
	createInfoRasterizationState' ::
		Maybe (RasterizationState.CreateInfo n6),
	createInfoMultisampleState' :: Maybe (MultisampleState.CreateInfo n7),
	createInfoDepthStencilState' :: Maybe (DepthStencilState.CreateInfo n8),
	createInfoColorBlendState' :: Maybe (ColorBlendState.CreateInfo n9),
	createInfoDynamicState' :: Maybe (DynamicState.CreateInfo n10),
	createInfoLayout' :: Layout.L,
	createInfoRenderPass' :: RenderPass.R,
	createInfoSubpass' :: Word32,
	createInfoBasePipelineHandle' :: V2 G vsts',
	createInfoBasePipelineIndex' :: Int32 }

maybeToCore :: (a -> ContT r IO (Ptr b)) -> Maybe a -> ContT r IO (Ptr b)
maybeToCore f = \case Nothing -> return NullPtr; Just x -> f x

createInfoToCore' :: (
	Pointable n,
	ShaderStage.CreateInfoListToCore' nskndvss,
	Pointable n2, Pointable n3, Pointable n4,
	Pointable n5, Pointable n6, Pointable n7, Pointable n8, Pointable n9,
	Pointable n10,
	BindingStrideList.BindingStrideList
		vs VertexInput.Rate VertexInput.Rate,
	VertexInputState.CreateInfoAttributeDescription vs ts ) =>
	CreateInfo' n nskndvss '(n2, vs, ts) n3 n4 n5 n6 n7 n8 n9 n10 vsts' ->
	ContT r IO C.CreateInfo
createInfoToCore' CreateInfo' {
	createInfoNext' = mnxt,
	createInfoFlags' = CreateFlagBits flgs,
	createInfoStages' = ss,
	createInfoVertexInputState' = ((\(V3 x) -> x) <$>) -> mvist,
	createInfoInputAssemblyState' = miast,
	createInfoTessellationState' = mtst,
	createInfoViewportState' = mvst,
	createInfoRasterizationState' = mrst,
	createInfoMultisampleState' = mmst,
	createInfoDepthStencilState' = mdsst,
	createInfoColorBlendState' = mcbst,
	createInfoDynamicState' = mdst,
	createInfoLayout' = Layout.L lyt,
	createInfoRenderPass' = RenderPass.R rp,
	createInfoSubpass' = sp,
	createInfoBasePipelineHandle' = V2 bph,
	createInfoBasePipelineIndex' = bpi
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	css <- ShaderStage.createInfoListToCore' ss
	let	sc = length css
	pss <- ContT $ allocaArray sc
	lift $ pokeArray pss css
	pvist <- maybeToCore VertexInputState.createInfoToCore mvist
	piast <- maybeToCore InputAssemblyState.createInfoToCore miast
	ptst <- maybeToCore TessellationState.createInfoToCore mtst
	pvst <- maybeToCore ViewportState.createInfoToCore mvst
	prst <- maybeToCore RasterizationState.createInfoToCore mrst
	pmst <- maybeToCore MultisampleState.createInfoToCore mmst
	pdsst <- maybeToCore DepthStencilState.createInfoToCore mdsst
	pcbst <- maybeToCore ColorBlendState.createInfoToCore mcbst
	pdst <- maybeToCore DynamicState.createInfoToCore mdst
	bph' <- lift $ gToCore bph
	pure C.CreateInfo {
		C.createInfoSType = (),
		C.createInfoPNext = pnxt,
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

type CreateInfo'' = V12 CreateInfo'

class CreateInfoListToCore' sss where
	createInfoListToCore' ::
		HeteroVarList CreateInfo'' sss -> ContT r IO [C.CreateInfo]

instance CreateInfoListToCore' '[] where createInfoListToCore' HVNil = pure []

instance (
	Pointable n, Pointable n2, Pointable n3, Pointable n4, Pointable n5,
	Pointable n6, Pointable n7, Pointable n8, Pointable n9, Pointable n10,
	ShaderStage.CreateInfoListToCore' nskndvss,
	BindingStrideList.BindingStrideList
		vs VertexInput.Rate VertexInput.Rate,
	VertexInputState.CreateInfoAttributeDescription vs ts,
	CreateInfoListToCore' ss ) =>
	CreateInfoListToCore' ('(
		n, nskndvss, '(n2, vs, ts), n3, n4, n5, n6, n7, n8, n9, n10, vsts' ) ': ss) where
	createInfoListToCore' (V12 ci :...: cis) = (:)
		<$> createInfoToCore' ci
		<*> createInfoListToCore' cis

gNull :: IO (G vs ts)
gNull = G <$> newIORef NullHandle

newtype G (vs :: [Type]) (ts :: [(Nat, Type)]) = G (IORef Pipeline.C.P)

gToCore :: G vs ts -> IO Pipeline.C.P
gToCore (G rp) = readIORef rp

gFromCore :: Pipeline.C.P -> IO (G vs ts)
gFromCore p = G <$> newIORef p

type G' = V2 G

class GListFromCore vstss where
	gListFromCore :: [Pipeline.C.P] -> IO (HeteroVarList G' vstss)
	gListToIORefs :: HeteroVarList G' vstss -> [IORef Pipeline.C.P]

gListToCore :: GListFromCore vstss =>
	HeteroVarList G' vstss -> IO [Pipeline.C.P]
gListToCore cps = readIORef `mapM` gListToIORefs cps

instance GListFromCore '[] where
	gListFromCore [] = pure HVNil
	gListFromCore _ = error "bad"
	gListToIORefs HVNil = []
	
instance GListFromCore vstss =>
	GListFromCore ('(vs, ts) ': vstss) where
	gListFromCore [] = error "bad"
	gListFromCore (cp : cps) = (:...:) <$> (V2 <$> gFromCore cp) <*> gListFromCore cps
	gListToIORefs (V2 (G cp) :...: cps) = cp : gListToIORefs cps

createGs' :: (
	CreateInfoListToCore' ss, Pointable n', GListFromCore (GListVars ss)
	) => Device.D -> Maybe Cache.C ->
	HeteroVarList (V12 CreateInfo') ss ->
	Maybe (AllocationCallbacks.A n') -> IO (HeteroVarList (V2 G) (GListVars ss))
createGs' dvc mc cis mac = gListFromCore =<< createRaw' dvc mc cis mac

recreateGs' :: (
	CreateInfoListToCore' ss, Pointable c, Pointable d,
	GListFromCore (GListVars ss) ) => Device.D -> Maybe Cache.C ->
	HeteroVarList (V12 CreateInfo') ss ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	HeteroVarList (V2 G) (GListVars ss) -> IO ()
recreateGs' dvc mc cis macc macd gs =
	recreateRaw' dvc mc cis macc macd $ gListToIORefs gs

type family GListVars (ss :: [(
		Type, [(Type, ShaderKind, Type)],
		(Type, [Type], [(Nat, Type)]), Type, Type, Type, Type, Type, Type, Type,
		Type, ([Type], [(Nat, Type)]))]) :: [([Type], [(Nat, Type)])] where
	GListVars '[] = '[]
	GListVars ('(
		n, nskndvss, '(n2, vs, ts),
		n3, n4, n5, n6, n7, n8, n9, n10, vsts' ) ': ss) = '(vs, ts) ': GListVars ss

createRaw' :: (CreateInfoListToCore' ss, Pointable n') =>
	Device.D -> Maybe Cache.C ->
	HeteroVarList CreateInfo'' ss ->
	Maybe (AllocationCallbacks.A n') -> IO [Pipeline.C.P]
createRaw' (Device.D dvc) mc cis mac = ($ pure) $ runContT do
	let	cc = case mc of Nothing -> NullPtr; Just (Cache.C c) -> c
	ccis <- createInfoListToCore' cis
	let	cic = length ccis
	pcis <- ContT $ allocaArray cic
	lift $ pokeArray pcis ccis
	pac <- AllocationCallbacks.maybeToCore mac
	pps <- ContT $ allocaArray cic
	lift do	r <- C.create dvc cc (fromIntegral cic) pcis pac pps
		throwUnlessSuccess $ Result r
		peekArray cic pps

recreateRaw' :: (CreateInfoListToCore' ss, Pointable c, Pointable d) =>
	Device.D -> Maybe Cache.C ->
	HeteroVarList CreateInfo'' ss ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	[IORef Pipeline.C.P] -> IO ()
recreateRaw' dvc mc cis macc macd rs = do
	os <- readIORef `mapM` rs
	ns <- createRaw' dvc mc cis macc
	zipWithM_ writeIORef rs ns
	(\o -> destroyRaw dvc o macd) `mapM_` os

destroyRaw :: Pointable d =>
	Device.D -> Pipeline.C.P -> Maybe (AllocationCallbacks.A d) -> IO ()
destroyRaw (Device.D dvc) p macd = ($ pure) $ runContT do
	pacd <- AllocationCallbacks.maybeToCore macd
	lift $ Pipeline.C.destroy dvc p pacd

destroy :: Pointable n =>
	Device.D -> G vs ts -> Maybe (AllocationCallbacks.A n) -> IO ()
destroy (Device.D dvc) g mac = ($ pure) $ runContT do
	p <- lift $ gToCore g
	pac <- AllocationCallbacks.maybeToCore mac
	lift $ Pipeline.C.destroy dvc p pac

destroyGs' :: (GListFromCore vstss, Pointable n) =>
	Device.D -> HeteroVarList (V2 G) vstss -> Maybe (AllocationCallbacks.A n) -> IO ()
destroyGs' dvc gs mac = ((\g -> gFromCore g >>= \g' -> destroy dvc g' mac) `mapM_`) =<< gListToCore gs
