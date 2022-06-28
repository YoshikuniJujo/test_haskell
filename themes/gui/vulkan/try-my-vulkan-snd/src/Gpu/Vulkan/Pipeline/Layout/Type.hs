{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Layout.Type where

import Data.Kind

import qualified Gpu.Vulkan.DescriptorSetLayout.Type as DescriptorSetLayout
import qualified Gpu.Vulkan.Pipeline.Layout.Middle as M

newtype L s = L M.L deriving Show

newtype LL s (sbtss :: [(Type, [DescriptorSetLayout.BindingType])]) = LL M.L deriving Show
