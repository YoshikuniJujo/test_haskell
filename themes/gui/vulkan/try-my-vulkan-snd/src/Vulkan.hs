{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan where

import qualified Vulkan.Middle as M
import qualified Vulkan.Semaphore as Semaphore
import qualified Vulkan.CommandBuffer.Type as CommandBuffer
import qualified Vulkan.Pipeline.Enum as Pipeline

data SubmitInfo n s vs = SubmitInfo {
	submitInfoNext :: Maybe n,
	submitInfoWaitSemaphoreDstStageMasks ::
		[(Semaphore.S, Pipeline.StageFlags)],
	submitInfoCommandBuffers :: [CommandBuffer.C s vs],
	submitInfoSignalSemaphores :: [Semaphore.S] }
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
