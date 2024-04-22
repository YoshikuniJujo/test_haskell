{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PhysicalDevice.Struct where

import Foreign.Ptr
import Foreign.Marshal.Alloc
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

import Data.TypeLevel.Maybe qualified as TMaybe

vkPhysicalDeviceLimits
vkPhysicalDeviceFeatures

featuresZero :: Features
featuresZero = unsafePerformIO $ featuresFromCore <$> C.getClearedFeatures

instance Default Features where def = featuresZero

makeStructure "DescriptorIndexingFeatures"

instance Peek DescriptorIndexingFeaturesNoNext where
	peek' = (descriptorIndexingFeaturesFromCore <$>) . peek . castPtr

instance Typeable DescriptorIndexingFeaturesNoNext where
	structureType = StructureTypePhysicalDeviceDescriptorIndexingFeatures

instance Sizable DescriptorIndexingFeaturesNoNext where
	sizeOf' = sizeOf @C.DescriptorIndexingFeatures undefined
	alignment' = alignment @C.DescriptorIndexingFeatures undefined

instance WithPoked (TMaybe.M mn) => WithPoked (DescriptorIndexingFeatures mn) where
	withPoked' difs f = alloca \pcdifs -> do
		descriptorIndexingFeaturesToCore difs $ \cdifs -> poke pcdifs cdifs
		f . ptrS $ castPtr pcdifs

instance Nextable DescriptorIndexingFeatures where
	nextableSize = sizeOf @C.DescriptorIndexingFeatures undefined
	nextableType = StructureTypePhysicalDeviceDescriptorIndexingFeatures
	nextPtr p = C.descriptorIndexingFeaturesPNext <$> peek (castPtr p)
	createNextable p n =
		descriptorIndexingFeaturesFromNoNext n .
		descriptorIndexingFeaturesFromCore <$> peek (castPtr p)
