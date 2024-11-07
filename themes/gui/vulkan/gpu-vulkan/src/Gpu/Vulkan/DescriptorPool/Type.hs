module Gpu.Vulkan.DescriptorPool.Type where

import qualified Gpu.Vulkan.DescriptorPool.Middle as M

newtype P s = P M.D deriving Show
