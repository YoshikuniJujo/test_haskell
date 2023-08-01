{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Fence.Type where

import qualified Gpu.Vulkan.Fence.Middle as M

newtype F sf = F M.F deriving Show
