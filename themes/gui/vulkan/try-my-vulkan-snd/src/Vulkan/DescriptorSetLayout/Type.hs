{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.DescriptorSetLayout.Type where

import qualified Vulkan.DescriptorSetLayout.Middle as M

newtype L s = L { unL :: M.L } deriving Show
