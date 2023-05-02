{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.AllocationCallbacks (
	createNew, M.FunctionsInfo(..), Functions, apply, A,
	M.FnAllocationFunction, M.FnReallocationFunction, M.FnFreeFunction,
	M.FnInternalAllocationNotification, M.FnInternalFreeNotification ) where

import Control.Exception
import Gpu.Vulkan.AllocationCallbacks.Type
import Gpu.Vulkan.AllocationCallbacks.Middle qualified as M

createNew :: M.FunctionsInfo a -> (forall s . Functions s a -> IO b) -> IO b
createNew fns f = bracket (M.createNew fns) M.destroyNew (f . Functions)
