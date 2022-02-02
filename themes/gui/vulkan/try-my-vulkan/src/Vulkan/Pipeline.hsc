{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Control.Monad.Cont
import Data.Kind
import Data.Word
import Data.Int

import Vulkan.Base
import Vulkan.Exception
import Vulkan.AllocationCallbacks
import Vulkan.Device
import Vulkan.Pipeline.Cache (PipelineCache(..))
import Vulkan.PipelineCreateFlagBits

import Vulkan.Pipeline.VertexInputState.BindingStrideList

import qualified Vulkan.Pipeline.Internal as I
import qualified Vulkan.Pipeline.ShaderStage as ShaderStage
import qualified Vulkan.Pipeline.VertexInputState as VertexInputState
import qualified Vulkan.Pipeline.InputAssemblyState as InputAssemblyState
import qualified Vulkan.Pipeline.TessellationState as TessellationState
import qualified Vulkan.Pipeline.ViewportState as ViewportState
import qualified Vulkan.Pipeline.RasterizationState as RasterizationState
import qualified Vulkan.Pipeline.MultisampleState as MultisampleState
import qualified Vulkan.Pipeline.DepthStencilState as DepthStencilState
import qualified Vulkan.Pipeline.ColorBlendState as ColorBlendState
import qualified Vulkan.Pipeline.DynamicState as DynamicState
import qualified Vulkan.Pipeline.Layout as Layout

import qualified Vulkan.Pipeline.VertexInputState.Internal as VertexInputState.I
import qualified Vulkan.Pipeline.InputAssemblyState.Internal as
	InputAssemblyState.I
import qualified Vulkan.Pipeline.TessellationState.Internal as
	TessellationState.I
import qualified Vulkan.Pipeline.ViewportState.Internal as ViewportState.I
import qualified Vulkan.Pipeline.RasterizationState.Internal as
	RasterizationState.I
import qualified Vulkan.Pipeline.MultisampleState.Internal as MultisampleState.I
import qualified Vulkan.Pipeline.DepthStencilState.Internal as
	DepthStencilState.I
import qualified Vulkan.Pipeline.ColorBlendState.Internal as ColorBlendState.I
import qualified Vulkan.Pipeline.DynamicState.Internal as DynamicState.I

import qualified Vulkan.AllocationCallbacks.Internal as I

#include <vulkan/vulkan.h>

data CreateInfo n n1 n2 vs ts n3 n4 n5 n6 n7 n8 n9 n10 = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: PipelineCreateFlags,
	createInfoStages :: [ShaderStage.CreateInfo n1],
	createInfoVertexInputState ::
		Maybe (VertexInputState.CreateInfo n2 vs ts),
	createInfoInputAssemblyState ::
		Maybe (InputAssemblyState.CreateInfo n3),
	createInfoTessellationState :: Maybe (TessellationState.CreateInfo n4),
	createInfoViewportState :: Maybe (ViewportState.CreateInfo n5),
	createInfoRasterizationState :: RasterizationState.CreateInfo n6,
	createInfoMultisampleState :: Maybe (MultisampleState.CreateInfo n7),
	createInfoDepthStencilState :: Maybe (DepthStencilState.CreateInfo n8),
	createInfoColorBlendState :: Maybe (ColorBlendState.CreateInfo n9),
	createInfoDynamicState :: Maybe (DynamicState.CreateInfo n10),
	createInfoLayout :: Layout.PipelineLayout,
	createInfoRenderPass :: RenderPass,
	createInfoSubpass :: Word32,
	createInfoBasePipelineHandle :: Pipeline vs ts,
	createInfoBasePipelineIndex :: Int32 }
	deriving Show

createInfoToC :: (
	Pointable n, Pointable n1, Pointable n2, Pointable n3, Pointable n4,
	Pointable n5, Pointable n6, Pointable n7, Pointable n8, Pointable n9,
	Pointable n10,
	BindingStrideList vs
		VertexInputState.VertexInputRate
		VertexInputState.I.VertexInputRate,
	VertexInputState.PipelineVertexInputStateCreateInfoAttributeDescription
		vs ts ) =>
	CreateInfo n n1 n2 vs ts n3 n4 n5 n6 n7 n8 n9 n10 ->
	(I.CreateInfo -> IO a) -> IO a
createInfoToC CreateInfo {
	createInfoNext = mnxt, createInfoFlags = flgs,
	createInfoStages = ss,
	createInfoVertexInputState = mvis,
	createInfoInputAssemblyState = mias,
	createInfoTessellationState = mts,
	createInfoViewportState = mvs,
	createInfoRasterizationState = rs,
	createInfoMultisampleState = mms,
	createInfoDepthStencilState = mdss,
	createInfoColorBlendState = mcbs,
	createInfoDynamicState = mds,
	createInfoLayout = lyt,
	createInfoRenderPass = rp,
	createInfoSubpass = word32ToUint32T -> sp,
	createInfoBasePipelineHandle = pipelineToC -> bph,
	createInfoBasePipelineIndex = int32ToInt32T -> bpi
	} = runContT do
	(castPtr -> pnxt) <- ContT $ withMaybePointer mnxt
	let	sc = length ss
	iss <- (ContT . ShaderStage.createInfoToC) `mapM` ss
	pss <- ContT $ allocaArray sc
	lift $ pokeArray pss iss
	pvis <- case mvis of
		Nothing -> pure NullPtr
		Just vis -> do
			VertexInputState.I.CreateInfo_ fvis <-
				ContT $ VertexInputState.createInfoToC vis
			ContT $ withForeignPtr fvis
	pias <- case mias of
		Nothing -> pure NullPtr
		Just ias -> do
			InputAssemblyState.I.CreateInfo_ fias <-
				ContT $ InputAssemblyState.createInfoToC ias
			ContT $ withForeignPtr fias
	pts <- case mts of
		Nothing -> pure NullPtr
		Just ts -> do
			TessellationState.I.CreateInfo_ fts <-
				ContT $ TessellationState.createInfoToC ts
			ContT $ withForeignPtr fts
	pvs <- case mvs of
		Nothing -> pure NullPtr
		Just vs -> do
			ViewportState.I.CreateInfo_ fvs <-
				ContT $ ViewportState.createInfoToC vs
			ContT $ withForeignPtr fvs
	prs <- do
		RasterizationState.I.CreateInfo_ frs <-
			ContT $ RasterizationState.createInfoToC rs
		ContT $ withForeignPtr frs
	pms <- case mms of
		Nothing -> pure NullPtr
		Just ms -> do
			MultisampleState.I.CreateInfo_ fms <-
				ContT $ MultisampleState.createInfoToC ms
			ContT $ withForeignPtr fms
	pdss <- case mdss of
		Nothing -> pure NullPtr
		Just dss -> do
			DepthStencilState.I.CreateInfo_ fdss <-
				ContT $ DepthStencilState.createInfoToC dss
			ContT $ withForeignPtr fdss
	pcbs <- case mcbs of
		Nothing -> pure NullPtr
		Just cbs -> do
			ColorBlendState.I.CreateInfo_ fcbs <-
				ContT $ ColorBlendState.createInfoToC cbs
			ContT $ withForeignPtr fcbs
	pds <- case mds of
		Nothing -> pure NullPtr
		Just ds -> do
			DynamicState.I.CreateInfo_ fds <-
				ContT $ DynamicState.createInfoToC ds
			ContT $ withForeignPtr fds
	pure I.CreateInfo {
		I.createInfoSType = (),
		I.createInfoPNext = pnxt,
		I.createInfoFlags = flgs,
		I.createInfoStageCount = fromIntegral sc,
		I.createInfoPStages = pss,
		I.createInfoPVertexInputState = pvis,
		I.createInfoPInputAssemblyState = pias,
		I.createInfoPTessellationState = pts,
		I.createInfoPViewportState = pvs,
		I.createInfoPRasterizationState = prs,
		I.createInfoPMultisampleState = pms,
		I.createInfoPDepthStencilState = pdss,
		I.createInfoPColorBlendState = pcbs,
		I.createInfoPDynamicState = pds,
		I.createInfoLayout = lyt,
		I.createInfoRenderPass = rp,
		I.createInfoSubpass = sp,
		I.createInfoBasePipelineHandle = bph,
		I.createInfoBasePipelineIndex = bpi }

createSingle :: (
	Pointable n, Pointable n1, Pointable n2, Pointable n3, Pointable n4,
	Pointable n5, Pointable n6, Pointable n7, Pointable n8, Pointable n9,
	Pointable n10, Pointable n11,
	BindingStrideList vs
		VertexInputState.VertexInputRate
		VertexInputState.I.VertexInputRate,
	VertexInputState.PipelineVertexInputStateCreateInfoAttributeDescription
		vs ts ) =>
	Device -> PipelineCache ->
	CreateInfo n n1 n2 vs ts n3 n4 n5 n6 n7 n8 n9 n10 ->
	Maybe (AllocationCallbacks n11) -> IO (Pipeline vs ts)
createSingle dvc pc ci mac = do
	pl :-: PLNil <- create dvc pc (ci :.: CINil) mac
	pure pl

create :: (
	CreateInfoListToICreateInfoList '(nss, vtss),
	PipelineCListToPipelineList vtss, Pointable n11 ) =>
	Device -> PipelineCache -> CreateInfoList '(nss, vtss) ->
	Maybe (AllocationCallbacks n11) -> IO (PipelineList vtss)
create dvc pc cis mac = ($ pure) . runContT $ createContT dvc pc cis mac

createContT :: (
	CreateInfoListToICreateInfoList '(nss, vtss),
	PipelineCListToPipelineList vtss, Pointable n11 ) =>
	Device -> PipelineCache -> CreateInfoList '(nss, vtss) ->
	Maybe (AllocationCallbacks n11) -> ContT r IO (PipelineList vtss)
createContT dvc pc cis mac = do
	cis' <- createInfoListToICreateInfoList cis
	plcs <- createGen dvc pc cis' mac
	pure $ pipelineCListToPipelineList plcs

createGen :: Pointable n11 => Device -> PipelineCache -> [I.CreateInfo] ->
	Maybe (AllocationCallbacks n11) -> ContT r IO [PipelineC]
createGen dvc pc cis mac = do
	let	cic = length cis
	pcis <- ContT $ allocaArray cic
	lift $ pokeArray pcis cis
	pac <- case mac of
		Nothing -> pure NullPtr
		Just ac -> ContT $ withAllocationCallbacksPtr ac
	ppl <- ContT alloca
	lift do	r <- c_vkCreateGraphicsPipelines
			dvc pc (fromIntegral cic) pcis pac ppl
		throwUnlessSuccess r
		peekArray cic ppl

foreign import ccall "vkCreateGraphicsPipelines" c_vkCreateGraphicsPipelines ::
	Device -> PipelineCache -> #{type uint32_t} -> Ptr I.CreateInfo ->
	Ptr I.AllocationCallbacks -> Ptr PipelineC -> IO Result

infixr 5 :.:

type Ns = (Type, Type, Type, Type, Type, Type, Type, Type, Type, Type, Type)

data CreateInfoList (as :: ([Ns], [(Type, [Type])])) where
	CINil :: CreateInfoList '( '[], '[])
	(:.:) :: CreateInfo n n1 n2 vs ts n3 n4 n5 n6 n7 n8 n9 n10 ->
		CreateInfoList '(nss, vtss) ->
		CreateInfoList '(
			'(n, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10) ': nss,
			'(vs, ts) ': vtss )

class CreateInfoListToICreateInfoList (as :: ([Ns], [(Type, [Type])])) where
	createInfoListToICreateInfoList ::
		CreateInfoList as -> ContT r IO [I.CreateInfo]

instance CreateInfoListToICreateInfoList '( '[], '[]) where
	createInfoListToICreateInfoList _ = pure []

instance (
	Pointable n, Pointable n1, Pointable n2, Pointable n3, Pointable n4,
	Pointable n5, Pointable n6, Pointable n7, Pointable n8, Pointable n9,
	Pointable n10,
	BindingStrideList vs
		VertexInputState.VertexInputRate
		VertexInputState.I.VertexInputRate,
	VertexInputState.PipelineVertexInputStateCreateInfoAttributeDescription
		vs ts,
	CreateInfoListToICreateInfoList '(nss, vtss) ) =>
	CreateInfoListToICreateInfoList '(
	'(n, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10) ': nss,
	'(vs, ts) ': vtss ) where
	createInfoListToICreateInfoList (ci :.: cis) = (:)
		<$> ContT (createInfoToC ci)
		<*> createInfoListToICreateInfoList cis

data PipelineList (vtss :: [(Type, [Type])]) where
	PLNil :: PipelineList '[]
	(:-:) :: Pipeline vs ts -> PipelineList vtss ->
		PipelineList ('(vs, ts) ': vtss)

class PipelineCListToPipelineList (vtss :: [(Type, [Type])]) where
	pipelineCListToPipelineList :: [PipelineC] -> PipelineList vtss

instance PipelineCListToPipelineList '[] where
	pipelineCListToPipelineList [] = PLNil
	pipelineCListToPipelineList _ = error "extra PipelineCs"

instance PipelineCListToPipelineList vtss =>
	PipelineCListToPipelineList ('(vs, ts) ': vtss) where
	pipelineCListToPipelineList [] = error "PipelineCs is not enough"
	pipelineCListToPipelineList (PipelineC p : plcs) =
		Pipeline p :-: pipelineCListToPipelineList plcs

destroy :: Pointable n =>
	Device -> Pipeline vs ts -> Maybe (AllocationCallbacks n) -> IO ()
destroy dvc pl mac = ($ pure) $ runContT do
	pac <- case mac of
		Nothing -> pure NullPtr
		Just ac -> ContT $ withAllocationCallbacksPtr ac
	lift $ c_vkDestroyPipeline dvc pl pac

foreign import ccall "vkDestroyPipeline" c_vkDestroyPipeline ::
	Device -> Pipeline vs ts -> Ptr I.AllocationCallbacks -> IO ()
