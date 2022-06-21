{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSetLayout.Type where

import qualified Gpu.Vulkan.DescriptorSetLayout.Middle as M

newtype L s = L { unL :: M.L } deriving Show
