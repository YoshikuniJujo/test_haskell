{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Ext.DebugUtils.Messenger (
	create, M, M.CreateInfo(..), M.FnCallback ) where

import Foreign.Storable
import Foreign.Pointable
import Control.Exception

import Gpu.Vulkan.Ext.DebugUtils.Messenger.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Instance.Type as Instance
import qualified Gpu.Vulkan.Ext.DebugUtils.Messenger.Middle as M

create :: (
	Pointable n, Storable n2, Storable n3, Storable n4, Storable n5,
	Storable ud, Pointable ud, Pointable c, Pointable d ) =>
	Instance.I si -> M.CreateInfo n n2 n3 n4 n5 ud ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall s . M s -> IO a) -> IO a
create (Instance.I ist) ci macc macd f = bracket
	(M.create ist ci macc) (\m -> M.destroy ist m macd) (f . M)
