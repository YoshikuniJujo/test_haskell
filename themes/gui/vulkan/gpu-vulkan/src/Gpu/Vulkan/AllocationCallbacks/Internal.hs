{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.AllocationCallbacks.Internal (

	-- * CREATE

	create, Functions, M.FunctionsInfo(..),

	-- ** Function Types

	M.FnAllocationFunction, M.FnReallocationFunction, M.FnFreeFunction,
	M.FnInternalAllocationNotification, M.FnInternalFreeNotification,

	-- *** Size and Alignment

	M.Size, M.Alignment,

	-- * APPLY

	apply, A, ToMiddle(..)

	) where

import Control.Exception
import Gpu.Vulkan.AllocationCallbacks.Type
import Gpu.Vulkan.AllocationCallbacks.Middle qualified as M

create :: M.FunctionsInfo a -> (forall s . Functions s a -> IO b) -> IO b
create fns f = bracket (M.create fns) M.destroy (f . Functions)
