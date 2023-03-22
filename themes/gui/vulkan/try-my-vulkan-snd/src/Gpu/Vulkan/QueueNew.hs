{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.QueueNew (
	M.Q, submit, M.waitIdle
	) where

import Data.TypeLevel.Uncurry
import Data.HeteroParList qualified as HeteroParList

import Gpu.VulkanNew

import qualified Gpu.Vulkan.Fence.Type as Fence

import Gpu.Vulkan.Queue.Middle qualified as M

submit :: SubmitInfoListToMiddle nssssvsss => M.Q ->
	HeteroParList.PL (U4 SubmitInfo) nssssvsss -> Maybe (Fence.F sf) -> IO ()
submit q sis mf =
	M.submit q (submitInfoListToMiddle sis) $ (\(Fence.F f) -> f) <$> mf
