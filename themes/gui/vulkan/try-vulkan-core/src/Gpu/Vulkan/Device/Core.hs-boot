{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device.Core where

import Foreign.Ptr

data DTag
type D = Ptr DTag

data MemoryTag
type Memory = Ptr MemoryTag
