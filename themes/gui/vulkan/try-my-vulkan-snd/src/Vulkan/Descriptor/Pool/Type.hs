module Vulkan.Descriptor.Pool.Type where

import Foreign.Ptr
import qualified Vulkan.Descriptor.Pool.Middle as M

newtype P s = P M.P deriving Show
