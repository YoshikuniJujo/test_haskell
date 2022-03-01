{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.PhysicalDevice.Struct where

import Vulkan.PhysicalDevice.Struct.Th
import qualified Vulkan.PhysicalDevice.Struct.Core as C

vkPhysicalDeviceLimits
vkPhysicalDeviceFeatures
