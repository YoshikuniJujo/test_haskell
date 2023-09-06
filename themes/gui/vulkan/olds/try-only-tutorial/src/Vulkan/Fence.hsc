{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Fence where

import Foreign.Ptr

data FenceTag
type Fence = Ptr FenceTag
