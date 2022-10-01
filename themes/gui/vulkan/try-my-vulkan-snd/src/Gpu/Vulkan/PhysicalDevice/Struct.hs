{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PhysicalDevice.Struct where

import Data.Default
import System.IO.Unsafe

import Gpu.Vulkan.PhysicalDevice.Struct.Th
import qualified Gpu.Vulkan.PhysicalDevice.Core as C
import qualified Gpu.Vulkan.PhysicalDevice.Struct.Core as C

vkPhysicalDeviceLimits
vkPhysicalDeviceFeatures

featuresZero :: Features
featuresZero = unsafePerformIO $ featuresFromCore <$> C.getClearedFeatures

instance Default Features where def = featuresZero
