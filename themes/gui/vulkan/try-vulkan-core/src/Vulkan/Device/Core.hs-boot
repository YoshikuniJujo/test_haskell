{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Device.Core where

import Foreign.Ptr

data DTag
type D = Ptr DTag

data MemoryTag
type Memory = Ptr MemoryTag
