{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanPhysicalDeviceStruct (make) where

import qualified MakeStruct

moduleName :: String
moduleName = "Gpu.Vulkan.PhysicalDevice.Struct.Core"

hsName :: [String]
hsName = ["Limits", "Features", "DescriptorIndexingFeatures"]

make :: IO ()
make = MakeStruct.make moduleName hsName "type PtrFeatures = Ptr Features"
