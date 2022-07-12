{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Graphics.Middle where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Pointable
import Control.Monad.Cont
import Data.Kind
import Data.HeteroList hiding (length)
import Data.Word
import Data.Int

import Shaderc.EnumAuto

import Gpu.Vulkan.Base
import Gpu.Vulkan.Exception
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Pipeline.Enum

import qualified Gpu.Vulkan.Pipeline.Core as Pipeline.C
import qualified Gpu.Vulkan.Pipeline.ShaderStage.Middle as ShaderStage
import qualified Gpu.Vulkan.Pipeline.VertexInputState as VertexInputState
import qualified Gpu.Vulkan.Pipeline.InputAssemblyState as InputAssemblyState
import qualified Gpu.Vulkan.Pipeline.TessellationState as TessellationState
import qualified Gpu.Vulkan.Pipeline.ViewportState as ViewportState
import qualified Gpu.Vulkan.Pipeline.RasterizationState as RasterizationState
import qualified Gpu.Vulkan.Pipeline.MultisampleState as MultisampleState
import qualified Gpu.Vulkan.Pipeline.DepthStencilState as DepthStencilState
import qualified Gpu.Vulkan.Pipeline.ColorBlendState as ColorBlendState
import qualified Gpu.Vulkan.Pipeline.DynamicState as DynamicState
import qualified Gpu.Vulkan.Pipeline.Layout.Middle as Layout
import qualified Gpu.Vulkan.RenderPass.Middle as RenderPass
import qualified Gpu.Vulkan.Pipeline.Graphics.Core as C
import qualified Gpu.Vulkan.Pipeline.VertexInputState.BindingStrideList as BindingStrideList
import qualified Gpu.Vulkan.VertexInput as VertexInput

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Middle as Device
import qualified Gpu.Vulkan.Pipeline.Cache.Middle as Cache

data CreateInfo n n1 sknds vss n2 vs' ts n3 n4 n5 n6 n7 n8 n9 n10 vs'' ts' = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoStages :: ShaderStage.CreateInfoList n1 sknds vss,
	createInfoVertexInputState ::
		Maybe (VertexInputState.CreateInfo n2 vs' ts),
	createInfoInputAssemblyState ::
		Maybe (InputAssemblyState.CreateInfo n3),
	createInfoTessellationState :: Maybe (TessellationState.CreateInfo n4),
	createInfoViewportState :: Maybe (ViewportState.CreateInfo n5),
	createInfoRasterizationState ::
		Maybe (RasterizationState.CreateInfo n6),
	createInfoMultisampleState :: Maybe (MultisampleState.CreateInfo n7),
	createInfoDepthStencilState :: Maybe (DepthStencilState.CreateInfo n8),
	createInfoColorBlendState :: Maybe (ColorBlendState.CreateInfo n9),
	createInfoDynamicState :: Maybe (DynamicState.CreateInfo n10),
	createInfoLayout :: Layout.L,
	createInfoRenderPass :: RenderPass.R,
	createInfoSubpass :: Word32,
	createInfoBasePipelineHandle :: G vs'' ts',
	createInfoBasePipelineIndex :: Int32 }

deriving instance (
	Show n, Show n1, Show n2, Show n3, Show n4, Show n5, Show n6, Show n7,
	Show n8, Show n9, Show n10,
	Show (ShaderStage.CreateInfoList n1 sknds vss)
	) =>
	Show (CreateInfo n n1 sknds vss n2 vs' ts n3 n4 n5 n6 n7 n8 n9 n10 vs'' ts')

data CreateInfo' n nskndvss n2 vs ts n3 n4 n5 n6 n7 n8 n9 n10 vs' ts' = CreateInfo' {
	createInfoNext' :: Maybe n,
	createInfoFlags' :: CreateFlags,
	createInfoStages' :: HeteroVarList ShaderStage.CreateInfo' nskndvss,
	createInfoVertexInputState' ::
		Maybe (VertexInputState.CreateInfo n2 vs ts),
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
	createInfoBasePipelineHandle' :: G vs' ts',
	createInfoBasePipelineIndex' :: Int32 }

maybeToCore :: (a -> ContT r IO (Ptr b)) -> Maybe a -> ContT r IO (Ptr b)
maybeToCore f = \case Nothing -> return NullPtr; Just x -> f x

createInfoToCore :: (
	Pointable n, Pointable n1, Pointable n2, Pointable n3, Pointable n4,
	Pointable n5, Pointable n6, Pointable n7, Pointable n8, Pointable n9,
	Pointable n10,
	ShaderStage.CreateInfoListToCore n1 sknds vss,
	BindingStrideList.BindingStrideList
		vs' VertexInput.Rate VertexInput.Rate,
	VertexInputState.CreateInfoAttributeDescription vs' ts ) =>
	CreateInfo n n1 sknds vss n2 vs' ts n3 n4 n5 n6 n7 n8 n9 n10 vs'' ts' ->
	ContT r IO C.CreateInfo
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoStages = ss,
--	createInfoStages = length &&& id -> (sc, ss),
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
	createInfoBasePipelineHandle = G bph,
	createInfoBasePipelineIndex = bpi
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	css <- ShaderStage.createInfoListToCore ss
	let	sc = length css
--	css <- ShaderStage.createInfoToCore `mapM` ss
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
		C.createInfoBasePipelineHandle = bph,
		C.createInfoBasePipelineIndex = bpi }

createInfoToCore' :: (
	Pointable n,
	ShaderStage.CreateInfoListToCore' nskndvss,
	Pointable n2, Pointable n3, Pointable n4,
	Pointable n5, Pointable n6, Pointable n7, Pointable n8, Pointable n9,
	Pointable n10,
	BindingStrideList.BindingStrideList
		vs VertexInput.Rate VertexInput.Rate,
	VertexInputState.CreateInfoAttributeDescription vs ts ) =>
	CreateInfo' n nskndvss n2 vs ts n3 n4 n5 n6 n7 n8 n9 n10 vs' ts' ->
	ContT r IO C.CreateInfo
createInfoToCore' CreateInfo' {
	createInfoNext' = mnxt,
	createInfoFlags' = CreateFlagBits flgs,
	createInfoStages' = ss,
	createInfoVertexInputState' = mvist,
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
	createInfoBasePipelineHandle' = G bph,
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
		C.createInfoBasePipelineHandle = bph,
		C.createInfoBasePipelineIndex = bpi }

type CreateInfo'' = V15 CreateInfo'

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
		n, nskndvss, n2,
		vs, ts, n3, n4, n5, n6, n7, n8, n9, n10, vs', ts' ) ': ss) where
	createInfoListToCore' (V15 ci :...: cis) = (:)
		<$> createInfoToCore' ci
		<*> createInfoListToCore' cis

data CreateInfoList
	ns n1s skndss vsss n2s vs's tss n3s n4s n5s n6s n7s n8s n9s n10s vs''s ts's where
	CreateInfoNil :: CreateInfoList
		'[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[]
	CreateInfoCons ::
		CreateInfo n n1 sknds vss n2 vs' ts n3 n4 n5 n6 n7 n8 n9 n10 vs'' ts' ->
		CreateInfoList ns n1s skndss vsss
			n2s vs's tss n3s n4s n5s n6s n7s n8s n9s n10s vs''s ts's ->
		CreateInfoList (n ': ns)
			(n1 ': n1s) (sknds ': skndss) (vss ': vsss)
			(n2 ': n2s) (vs' ': vs's) (ts ': tss) (n3 ': n3s)
			(n4 ': n4s) (n5 ': n5s) (n6 ': n6s) (n7 ': n7s)
			(n8 ': n8s) (n9 ': n9s) (n10 ': n10s) (vs'' ': vs''s) (ts' ': ts's)

class CreateInfoListToCore
	ns n1s skndss vsss n2s vs's tss n3s n4s n5s n6s n7s n8s n9s n10s vs''s ts's where
	createInfoListToCore ::
		CreateInfoList ns n1s skndss vsss
			n2s vs's tss n3s n4s n5s n6s n7s n8s n9s n10s vs''s ts's ->
		ContT r IO [C.CreateInfo]

instance CreateInfoListToCore
	'[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] where
	createInfoListToCore _ = pure []

instance (
	Pointable n, Pointable n1, Pointable n2, Pointable n3, Pointable n4,
	Pointable n5, Pointable n6, Pointable n7, Pointable n8, Pointable n9,
	Pointable n10,
	ShaderStage.CreateInfoListToCore n1 sknds vss,
	BindingStrideList.BindingStrideList
		vs' VertexInput.Rate VertexInput.Rate,
	VertexInputState.CreateInfoAttributeDescription vs' ts,
	CreateInfoListToCore ns n1s skndss vsss
		n2s vs's tss n3s n4s n5s n6s n7s n8s n9s n10s vs''s ts's ) =>
	CreateInfoListToCore
		(n ': ns) (n1 ': n1s) (sknds ': skndss) (vss ': vsss)
		(n2 ': n2s) (vs' ': vs's) (ts ': tss) (n3 ': n3s) (n4 ': n4s)
		(n5 ': n5s) (n6 ': n6s) (n7 ': n7s) (n8 ': n8s) (n9 ': n9s)
		(n10 ': n10s) (vs'' ': vs''s) (ts' ': ts's) where
	createInfoListToCore (ci `CreateInfoCons` cis) = (:)
		<$> createInfoToCore ci
		<*> createInfoListToCore cis

pattern GNull :: G vs ts
pattern GNull <- G NullHandle where
	GNull = G NullHandle

newtype G vs (ts :: [Type]) = G Pipeline.C.P deriving Show

type G' = V2 G

data PList vss tss where
	PNil :: PList '[] '[]
	PCons :: G vs ts -> PList vss tss -> PList (vs ': vss) (ts ': tss)

deriving instance Show (PList vss tss)

class PListFromCore vss tss where
	pListFromCore :: [Pipeline.C.P] -> PList vss tss
	pListToCore :: PList vss tss -> [Pipeline.C.P]

instance PListFromCore '[] '[] where
	pListFromCore [] = PNil
	pListFromCore _ = error "bad"
	pListToCore _ = []

instance
	PListFromCore vss tss =>
	PListFromCore (vs ': vss) (ts ': tss) where
	pListFromCore [] = error "bad"
	pListFromCore (cp : cps) = G cp `PCons` pListFromCore cps
	pListToCore (G cp `PCons` gs) = cp : pListToCore gs

class GListFromCore vstss where
	gListFromCore :: [Pipeline.C.P] -> HeteroVarList G' vstss
	gListToCore :: HeteroVarList G' vstss -> [Pipeline.C.P]

instance GListFromCore '[] where
	gListFromCore [] = HVNil
	gListFromCore _ = error "bad"
	gListToCore HVNil = []
	
instance GListFromCore vstss =>
	GListFromCore ('(vs, ts) ': vstss) where
	gListFromCore [] = error "bad"
	gListFromCore (cp : cps) = V2 (G cp) :...: gListFromCore cps
	gListToCore (V2 (G cp) :...: cps) = cp : gListToCore cps

create :: (
	CreateInfoListToCore ns
		n1s skndss vsss n2s vs's tss n3s n4s n5s n6s n7s n8s n9s n10s vs''s ts's,
	PListFromCore vs's tss,
	Pointable n' ) =>
	Device.D -> Maybe Cache.C ->
	CreateInfoList ns
		n1s skndss vsss n2s vs's tss n3s n4s n5s n6s n7s n8s n9s n10s vs''s ts's ->
	Maybe (AllocationCallbacks.A n') -> IO (PList vs's tss)
create dvc mc cis mac = pListFromCore <$> createRaw dvc mc cis mac

createGs' :: (
	CreateInfoListToCore' ss, Pointable n', GListFromCore (GListVars ss)
	) => Device.D -> Maybe Cache.C ->
	HeteroVarList CreateInfo'' ss ->
	Maybe (AllocationCallbacks.A n') -> IO (HeteroVarList G' (GListVars ss))
createGs' dvc mc cis mac = gListFromCore <$> createRaw' dvc mc cis mac

type family GListVars (ss :: [(
		Type, [(Type, ShaderKind, Type)],
		Type, Type, [Type], Type, Type, Type, Type, Type, Type, Type,
		Type, Type, [Type])]) :: [(Type, [Type])] where
	GListVars '[] = '[]
	GListVars ('(
		n, nskndvss, n2, vs, ts,
		n3, n4, n5, n6, n7, n8, n9, n10, vs', ts' ) ': ss) = '(vs, ts) ': GListVars ss

createRaw :: (
	CreateInfoListToCore ns
		n1s skndss vsss n2s vs's tss n3s n4s n5s n6s n7s n8s n9s n10s vs''s ts's,
	Pointable n' ) =>
	Device.D -> Maybe Cache.C ->
	CreateInfoList ns
		n1s skndss vsss n2s vs's tss n3s n4s n5s n6s n7s n8s n9s n10s vs''s ts's ->
	Maybe (AllocationCallbacks.A n') -> IO [Pipeline.C.P]
createRaw (Device.D dvc) mc cis mac = ($ pure) $ runContT do
	let	cc = case mc of Nothing -> NullPtr; Just (Cache.C c) -> c
	ccis <- createInfoListToCore cis
	let	cic = length ccis
	pcis <- ContT $ allocaArray cic
	lift $ pokeArray pcis ccis
	pac <- AllocationCallbacks.maybeToCore mac
	pps <- ContT $ allocaArray cic
	lift do	r <- C.create dvc cc (fromIntegral cic) pcis pac pps
		throwUnlessSuccess $ Result r
		peekArray cic pps

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

destroy :: Pointable n =>
	Device.D -> G vs ts -> Maybe (AllocationCallbacks.A n) -> IO ()
destroy (Device.D dvc) (G p) mac = ($ pure) $ runContT do
	pac <- AllocationCallbacks.maybeToCore mac
	lift $ Pipeline.C.destroy dvc p pac

destroyGs :: (PListFromCore vs's tss, Pointable n) =>
	Device.D -> PList vs's tss -> Maybe (AllocationCallbacks.A n) -> IO ()
destroyGs dvc gs mac = (\g -> destroy dvc (G g) mac) `mapM_` pListToCore gs

destroyGs' :: (GListFromCore vstss, Pointable n) =>
	Device.D -> HeteroVarList G' vstss -> Maybe (AllocationCallbacks.A n) -> IO ()
destroyGs' dvc gs mac = (\g -> destroy dvc (G g) mac) `mapM_` gListToCore gs
