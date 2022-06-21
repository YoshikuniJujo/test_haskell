{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CommandBuffer.Core where

import Foreign.Ptr

data CTag
type C = Ptr CTag
type PtrC = Ptr C
