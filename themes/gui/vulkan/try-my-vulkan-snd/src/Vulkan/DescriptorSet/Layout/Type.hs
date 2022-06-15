{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.DescriptorSet.Layout.Type where

import qualified Vulkan.DescriptorSet.Layout.Middle as M

newtype L s = L { unL :: M.L } deriving Show
