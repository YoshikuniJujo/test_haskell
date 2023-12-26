{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Queue (

	-- * SUBMIT AND WAIT IDLE

	submit, M.waitIdle, M.Q,

	-- * TYPE SYNONYM

	Index,

	-- * ENUM

	module Gpu.Vulkan.Queue.Enum

	) where

import Data.TypeLevel.Tuple.Uncurry
import Data.HeteroParList qualified as HeteroParList
import Data.Word

import Gpu.Vulkan.Internal

import qualified Gpu.Vulkan.Fence.Type as Fence

import Gpu.Vulkan.Queue.Middle qualified as M
import Gpu.Vulkan.Queue.Enum

submit :: SubmitInfoListToMiddle sias => M.Q ->
	HeteroParList.PL (U4 SubmitInfo) sias -> Maybe (Fence.F sf) -> IO ()
submit q sis mf =
	M.submit q (submitInfoListToMiddle sis) $ (\(Fence.F f) -> f) <$> mf

type Index = Word32
