{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PhysicalDevice.Struct where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.PeekPoke

import Data.Default
import System.IO.Unsafe

import Gpu.Vulkan.PhysicalDevice.Struct.Th
import qualified Gpu.Vulkan.PhysicalDevice.Core as C
import qualified Gpu.Vulkan.PhysicalDevice.Struct.Core as C

import Gpu.Vulkan.PhysicalDevice.Struct.ThTest

import Gpu.Vulkan.Enum
import Gpu.Vulkan.PNext.Middle.Internal

vkPhysicalDeviceLimits
vkPhysicalDeviceFeatures

featuresZero :: Features
featuresZero = unsafePerformIO $ featuresFromCore <$> C.getClearedFeatures

instance Default Features where def = featuresZero

makeStructure "DescriptorIndexingFeatures"

instance Peek DescriptorIndexingFeaturesNoNext where
	peek' = (descriptorIndexingFeaturesFromCore <$>) . peek . castPtr

instance Nextable DescriptorIndexingFeaturesNoNext where
	nextableType = StructureTypePhysicalDeviceDescriptorIndexingFeatures

instance Sizable DescriptorIndexingFeaturesNoNext where
	sizeOf' = sizeOf @C.DescriptorIndexingFeatures undefined
