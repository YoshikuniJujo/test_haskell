{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PhysicalDevice.Struct where

import Gpu.Vulkan.PhysicalDevice.Struct.Th
import qualified Gpu.Vulkan.PhysicalDevice.Struct.Core as C

vkPhysicalDeviceLimits
vkPhysicalDeviceFeatures
