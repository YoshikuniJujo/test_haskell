{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.AllocationCallbacks (
	create, A, M.Functions(..),
	M.FnAllocationFunction, M.FnReallocationFunction, M.FnFreeFunction,
	M.FnInternalAllocationNotification, M.FnInternalFreeNotification ) where

import Control.Exception
import Gpu.Vulkan.AllocationCallbacks.Type
import Gpu.Vulkan.AllocationCallbacks.Middle qualified as M

create :: M.Functions a -> (forall s . A s a -> IO b) -> IO b
create fns f = bracket (M.create fns) M.destroy (f . A)
