{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Control.Monad.Cont
import Data.Word
import Data.Int

import Vulkan.Base
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
import qualified Vulkan.RenderPass as RenderPass

import qualified Vulkan.Pipeline.VertexInputState.Internal as VertexInputState.I
import qualified Vulkan.Pipeline.InputAssemblyState.Internal as
	InputAssemblyState.I
import qualified Vulkan.Pipeline.TessellationState.Internal as
	TessellationState.I

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
	createInfoRenderPass :: RenderPass.RenderPass,
	createInfoSubpass :: Word32,
	createInfoBasePipelineHandle :: Pipeline,
	createInfoBasePipelineIndex :: Int32 }
	deriving Show

word32ToUint32T :: Word32 -> #{type uint32_t}
word32ToUint32T = fromIntegral

int32ToInt32T :: Int32 -> #{type int32_t}
int32ToInt32T = fromIntegral

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
	createInfoTessellationState = mts
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
	pure I.CreateInfo {
		I.createInfoSType = (),
		I.createInfoPNext = pnxt,
		I.createInfoFlags = flgs,
		I.createInfoStageCount = fromIntegral sc,
		I.createInfoPStages = pss,
		I.createInfoPVertexInputState = pvis,
		I.createInfoPInputAssemblyState = pias,
		I.createInfoPTessellationState = pts
		}
