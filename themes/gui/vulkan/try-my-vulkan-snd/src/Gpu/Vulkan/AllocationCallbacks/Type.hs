{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.AllocationCallbacks.Type where

import Gpu.Vulkan.AllocationCallbacks.Middle qualified as M

newtype A s a = A { toMiddle :: M.A a } deriving Show
