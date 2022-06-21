{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Fence.Core where

import Foreign.Ptr

data FTag
type F = Ptr FTag
