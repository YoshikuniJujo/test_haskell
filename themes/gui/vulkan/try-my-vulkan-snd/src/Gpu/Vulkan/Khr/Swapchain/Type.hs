{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Swapchain.Type where

import qualified Gpu.Vulkan.Khr.Swapchain.Middle as M

newtype S ss = S M.S deriving Show
