{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Surface.PhysicalDevice (

	-- * GET SUPPORT, FORMATS, CAPABILITIES AND PRESENT MODES

	getSupport, getFormats, getCapabilities, getPresentModes

	) where

import Gpu.Vulkan.Khr.Enum
import Gpu.Vulkan.Khr.Surface.Type

import qualified Gpu.Vulkan.PhysicalDevice as PhysicalDevice
import qualified Gpu.Vulkan.QueueFamily as QueueFamily
import qualified Gpu.Vulkan.Khr.Surface.Middle.Internal as M
import qualified Gpu.Vulkan.Khr.Surface.PhysicalDevice.Middle as M

getSupport :: PhysicalDevice.P -> QueueFamily.Index -> S ss -> IO Bool
getSupport phdvc qfi (S sfc) = M.getSupport phdvc qfi sfc

getCapabilities :: PhysicalDevice.P -> S ss -> IO M.Capabilities
getCapabilities phdvc (S sfc) = M.getCapabilities phdvc sfc

getFormats :: PhysicalDevice.P -> S ss -> IO [M.Format]
getFormats phdvc (S sfc) = M.getFormats phdvc sfc

getPresentModes :: PhysicalDevice.P -> S ss -> IO [PresentMode]
getPresentModes phdvc (S sfc) = M.getPresentModes phdvc sfc
