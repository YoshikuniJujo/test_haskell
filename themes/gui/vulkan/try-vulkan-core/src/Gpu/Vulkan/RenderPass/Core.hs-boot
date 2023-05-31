module Gpu.Vulkan.RenderPass.Core (R) where

import Foreign.Ptr

data RTag
type R = Ptr RTag
