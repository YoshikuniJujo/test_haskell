{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet where

import Data.Kind

import qualified Gpu.Vulkan.DescriptorSetLayout.Type as Layout

type LayoutArg = (Type, [Layout.BindingType])
