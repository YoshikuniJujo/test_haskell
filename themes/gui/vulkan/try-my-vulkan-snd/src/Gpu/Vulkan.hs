{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
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

data SemaphorePipelineStageFlags ss =
	SemaphorePipelineStageFlags (Semaphore.S ss) Pipeline.StageFlags
	deriving Show

-- deriving instance Show (HeteroVarList Semaphore.S ss)

semaphorePipelineStageFlagsToMiddle ::
	HeteroVarList SemaphorePipelineStageFlags sss ->
	[(Semaphore.M.S, Pipeline.StageFlags)]
semaphorePipelineStageFlagsToMiddle = heteroVarListToList
	\(SemaphorePipelineStageFlags (Semaphore.S s) psfs) -> (s, psfs)

data SubmitInfo n sss s vs = SubmitInfo {
	submitInfoNext :: Maybe n,
	submitInfoWaitSemaphoreDstStageMasks ::
		HeteroVarList SemaphorePipelineStageFlags sss,
--		[(Semaphore.M.S, Pipeline.StageFlags)],
	submitInfoCommandBuffers :: [CommandBuffer.C s vs],
	submitInfoSignalSemaphores :: [Semaphore.M.S] }

deriving instance (Show n, Show (HeteroVarList SemaphorePipelineStageFlags sss)) =>
	Show (SubmitInfo n sss s vs)

submitInfoToMiddle :: SubmitInfo n sss s vs -> M.SubmitInfo n vs
submitInfoToMiddle SubmitInfo {
	submitInfoNext = mnxt,
	submitInfoWaitSemaphoreDstStageMasks =
		semaphorePipelineStageFlagsToMiddle -> wsdsms,
	submitInfoCommandBuffers = (CommandBuffer.unC <$>) -> cbs,
	submitInfoSignalSemaphores = ssmprs } = M.SubmitInfo {
	M.submitInfoNext = mnxt,
	M.submitInfoWaitSemaphoreDstStageMasks = wsdsms,
	M.submitInfoCommandBuffers = cbs,
	M.submitInfoSignalSemaphores = ssmprs }

class SemaphorePipelineStageFlagsFromMiddle sss where
	semaphorePipelineStageFlagsFromMiddle ::
		[(Semaphore.M.S, Pipeline.StageFlags)] ->
		HeteroVarList SemaphorePipelineStageFlags sss

instance SemaphorePipelineStageFlagsFromMiddle '[] where
	semaphorePipelineStageFlagsFromMiddle [] = HVNil

instance SemaphorePipelineStageFlagsFromMiddle sss =>
	SemaphorePipelineStageFlagsFromMiddle (ss ': sss) where
	semaphorePipelineStageFlagsFromMiddle ((s, psfs) : spsfss) =
		SemaphorePipelineStageFlags (Semaphore.S s) psfs :...:
		semaphorePipelineStageFlagsFromMiddle spsfss

submitInfoFromMiddle ::
	SemaphorePipelineStageFlagsFromMiddle sss =>
	M.SubmitInfo n vs -> SubmitInfo n sss s vs
submitInfoFromMiddle M.SubmitInfo {
	M.submitInfoNext = mnxt,
	M.submitInfoWaitSemaphoreDstStageMasks =
		semaphorePipelineStageFlagsFromMiddle -> wsdsms,
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
