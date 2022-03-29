{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.DescriptorSet.Layout where

import qualified Vulkan.DescriptorSet.Layout.Core as C

newtype L = L C.L deriving Show
