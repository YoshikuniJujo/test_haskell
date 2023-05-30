{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device.Core (D) where

import Foreign.Ptr

data DTag
type D = Ptr DTag
