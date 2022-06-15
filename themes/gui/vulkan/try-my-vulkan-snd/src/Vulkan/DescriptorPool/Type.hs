module Vulkan.DescriptorPool.Type where

import Foreign.Ptr
import qualified Vulkan.DescriptorPool.Middle as M

newtype P s = P M.P deriving Show
