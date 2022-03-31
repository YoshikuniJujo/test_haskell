{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Data.Word
import Data.Int

import Vulkan.Pipeline.Enum

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
import qualified Vulkan.Pipeline.Core as C
import qualified Vulkan.Specialization as Specialization
import qualified Vulkan.Pipeline.VertexInputState.BindingStrideList as BindingStrideList
import qualified Vulkan.VertexInput as VertexInput

data CreateInfo n n1 sknd vs n2 vs' ts n3 n4 n5 n6 n7 n8 n9 n10 = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoStages :: [ShaderStage.CreateInfo n1 sknd vs],
	createInfoVertexInputState ::
		Maybe (VertexInputState.CreateInfo n2 vs' ts),
	createInfoInputAssemblyState ::
		Maybe (InputAssemblyState.CreateInfo n3),
	createInfoTessellationState :: Maybe (TessellationState.CreateInfo n4),
	createInfoViewportState :: Maybe (ViewportState.CreateInfo n5),
	createInfoRasterizationState ::
		Maybe (RasterizationState.CreateInfo n6),
	createInfoMultisampleState :: Maybe (MultisampleState.CreateInfo n7),
	creaetInfoDepthStencilState :: Maybe (DepthStencilState.CreateInfo n8),
	createInfoColorBlendState :: Maybe (ColorBlendState.CreateInfo n9),
	createInfoDynamicState :: Maybe (DynamicState.CreateInfo n10),
	createInfoLayout :: Layout.L,
	createInfoRenderPass :: RenderPass.R,
	createInfoSubpass :: Word32,
	createInfoBasePipelineHandle :: P,
	createInfoBasePipelineIndex :: Int32 }
	deriving Show

maybeToCore :: (a -> ContT r IO (Ptr b)) -> Maybe a -> ContT r IO (Ptr b)
maybeToCore f = \case Nothing -> return NullPtr; Just x -> f x

createInfoToCore :: (
	Pointable n, Pointable n1, Pointable n2, Pointable n3, Pointable n4,
	Pointable n5, Pointable n6, Pointable n7, Pointable n8, Pointable n9,
	Pointable n10,
	Specialization.StoreValues vs,
	BindingStrideList.BindingStrideList
		vs' VertexInput.Rate VertexInput.Rate,
	VertexInputState.CreateInfoAttributeDescription vs' ts ) =>
	CreateInfo n n1 sknd vs n2 vs' ts n3 n4 n5 n6 n7 n8 n9 n10 ->
	ContT r IO C.CreateInfo
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoStages = length &&& id -> (sc, ss),
	createInfoVertexInputState = mvist
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	css <- ShaderStage.createInfoToCore `mapM` ss
	pss <- ContT $ allocaArray sc
	pvist <- maybeToCore VertexInputState.createInfoToCore mvist
	lift $ pokeArray pss css
	pure C.CreateInfo {
		C.createInfoSType = (),
		C.createInfoPNext = pnxt,
		C.createInfoFlags = flgs,
		C.createInfoStageCount = fromIntegral sc,
		C.createInfoPStages = pss,
		C.createInfoPVertexInputState = pvist
		}

newtype P = P C.P deriving Show
