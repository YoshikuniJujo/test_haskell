{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan where

import Data.Kind
import Data.TypeLevel
import Data.HeteroParList

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

data SubmitInfo n sss svss ssss = SubmitInfo {
	submitInfoNext :: Maybe n,
	submitInfoWaitSemaphoreDstStageMasks ::
		HeteroVarList SemaphorePipelineStageFlags sss,
	submitInfoCommandBuffers :: HeteroVarList (V2 CommandBuffer.C) svss,
	submitInfoSignalSemaphores ::
		HeteroVarList Semaphore.S ssss }

-- deriving instance (Show n, Show (HeteroVarList SemaphorePipelineStageFlags sss)) =>
--	Show (SubmitInfo n sss s vs)

class CommandBufferListToMiddle svss where
	type CommandBufferListToMiddleMapSnd svss :: [[Type]]
	commandBufferListToMiddle ::
		HeteroVarList (V2 CommandBuffer.C) svss ->
		HeteroVarList CommandBuffer.CC
			(CommandBufferListToMiddleMapSnd svss)

instance CommandBufferListToMiddle '[] where
	type CommandBufferListToMiddleMapSnd '[] = '[]
	commandBufferListToMiddle HVNil = HVNil

instance CommandBufferListToMiddle svss =>
	CommandBufferListToMiddle ('(s, vs) ': svss) where
	type CommandBufferListToMiddleMapSnd ('(s, vs) ': svss) =
		vs ': CommandBufferListToMiddleMapSnd svss
	commandBufferListToMiddle (V2 (CommandBuffer.C cb) :...: cbs) =
		cb :...: commandBufferListToMiddle cbs

submitInfoToMiddle :: CommandBufferListToMiddle svss =>
	SubmitInfo n sss svss ssss ->
	M.SubmitInfo n (CommandBufferListToMiddleMapSnd svss)
submitInfoToMiddle SubmitInfo {
	submitInfoNext = mnxt,
	submitInfoWaitSemaphoreDstStageMasks =
		semaphorePipelineStageFlagsToMiddle -> wsdsms,
	submitInfoCommandBuffers = commandBufferListToMiddle -> cbs,
	submitInfoSignalSemaphores =
		heteroVarListToList (\(Semaphore.S s) -> s) -> ssmprs
	} = M.SubmitInfo {
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
