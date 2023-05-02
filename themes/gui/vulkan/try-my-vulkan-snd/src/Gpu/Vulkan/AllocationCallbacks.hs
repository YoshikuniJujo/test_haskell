{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.AllocationCallbacks (
	create, T.A, M.Functions(..),
	M.FnAllocationFunction, M.FnReallocationFunction, M.FnFreeFunction,
	M.FnInternalAllocationNotification, M.FnInternalFreeNotification,

	createNew, T.FunctionsT, T.apply
	) where

import Control.Exception
import Gpu.Vulkan.AllocationCallbacks.Type qualified as T
import Gpu.Vulkan.AllocationCallbacks.Middle qualified as M

create :: M.Functions a -> (forall s . T.A s a -> IO b) -> IO b
create fns f = bracket (M.create fns) M.destroy (f . T.A)

createNew :: M.Functions a -> (forall s . T.FunctionsT s a -> IO b) -> IO b
createNew fns f = bracket (M.createNew fns) M.destroyNew (f . T.FunctionsT)
