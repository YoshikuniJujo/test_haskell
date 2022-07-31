{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan where

import Data.HeteroList

import Gpu.Vulkan.Enum

import qualified Gpu.Vulkan.Middle as M
import qualified Gpu.Vulkan.Core as C
import qualified Gpu.Vulkan.Semaphore as Semaphore
import qualified Gpu.Vulkan.Semaphore.Middle as Semaphore.M
import qualified Gpu.Vulkan.CommandBuffer.Type as CommandBuffer
import qualified Gpu.Vulkan.Pipeline.Enum as Pipeline

data SubmitInfo n s vs = SubmitInfo {
	submitInfoNext :: Maybe n,
	submitInfoWaitSemaphoreDstStageMasks ::
		[(Semaphore.M.S, Pipeline.StageFlags)],
	submitInfoCommandBuffers :: [CommandBuffer.C s vs],
	submitInfoSignalSemaphores :: [Semaphore.M.S] }
	deriving Show

submitInfoToMiddle :: SubmitInfo n s vs -> M.SubmitInfo n vs
submitInfoToMiddle SubmitInfo {
	submitInfoNext = mnxt,
	submitInfoWaitSemaphoreDstStageMasks = wsdsms,
	submitInfoCommandBuffers = (CommandBuffer.unC <$>) -> cbs,
	submitInfoSignalSemaphores = ssmprs } = M.SubmitInfo {
	M.submitInfoNext = mnxt,
	M.submitInfoWaitSemaphoreDstStageMasks = wsdsms,
	M.submitInfoCommandBuffers = cbs,
	M.submitInfoSignalSemaphores = ssmprs }

submitInfoFromMiddle :: M.SubmitInfo n vs -> SubmitInfo n s vs
submitInfoFromMiddle M.SubmitInfo {
	M.submitInfoNext = mnxt,
	M.submitInfoWaitSemaphoreDstStageMasks = wsdsms,
	M.submitInfoCommandBuffers = (CommandBuffer.C <$>) -> cbs,
	M.submitInfoSignalSemaphores = ssmprs } = SubmitInfo {
	submitInfoNext = mnxt,
	submitInfoWaitSemaphoreDstStageMasks = wsdsms,
	submitInfoCommandBuffers = cbs,
	submitInfoSignalSemaphores = ssmprs }

data FormatProperties = FormatProperties {
	formatPropertiesLinearTilingFeatures :: FormatFeatureFlags,
	formatPropertiesOptimalTilingFeatures :: FormatFeatureFlags,
	formatPropertiesBufferFeatures :: FormatFeatureFlags }
	deriving Show

formatPropertiesFromCore :: C.FormatProperties -> FormatProperties
formatPropertiesFromCore C.FormatProperties {
	C.formatPropertiesLinearTilingFeatures = ltfs,
	C.formatPropertiesOptimalTilingFeatures = otfs,
	C.formatPropertiesBufferFeatures = bfs
	} = FormatProperties {
		formatPropertiesLinearTilingFeatures =
			FormatFeatureFlagBits ltfs,
		formatPropertiesOptimalTilingFeatures =
			FormatFeatureFlagBits otfs,
		formatPropertiesBufferFeatures = FormatFeatureFlagBits bfs }
