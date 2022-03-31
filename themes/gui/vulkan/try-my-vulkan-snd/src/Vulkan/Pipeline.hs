{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Pointable
import Control.Monad.Cont
import Data.Word
import Data.Int

import Vulkan.Base
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
import qualified Vulkan.Pipeline.VertexInputState.BindingStrideList as BindingStrideList
import qualified Vulkan.VertexInput as VertexInput

data CreateInfo n n1 sknds vss n2 vs' ts n3 n4 n5 n6 n7 n8 n9 n10 = CreateInfo {
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
	createInfoBasePipelineHandle :: P,
	createInfoBasePipelineIndex :: Int32 }

deriving instance (
	Show n, Show n1, Show n2, Show n3, Show n4, Show n5, Show n6, Show n7,
	Show n8, Show n9, Show n10,
	Show (ShaderStage.CreateInfoList n1 sknds vss)
	) =>
	Show (CreateInfo n n1 sknds vss n2 vs' ts n3 n4 n5 n6 n7 n8 n9 n10)

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
	CreateInfo n n1 sknds vss n2 vs' ts n3 n4 n5 n6 n7 n8 n9 n10 ->
	ContT r IO (Ptr C.CreateInfo)
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
	createInfoBasePipelineHandle = P bph,
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
	let	C.CreateInfo_ fCreateInfo = C.CreateInfo {
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
	ContT $ withForeignPtr fCreateInfo

newtype P = P C.P deriving Show

pattern PNull :: P
pattern PNull <- P NullHandle where
	PNull = P NullHandle
