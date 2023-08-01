{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CommandPool.Type where

import qualified Gpu.Vulkan.CommandPool.Middle as M

newtype C s = C M.C deriving Show
