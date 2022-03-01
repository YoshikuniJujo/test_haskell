{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.PhysicalDevice.Core where

import Foreign.Ptr
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Struct
import Data.Word
import Data.Int

import qualified Data.Text as T

import Vulkan.Base
import Vulkan.Instance.Core (Instance)
import Vulkan.Khr.Surface (Surface)

import Vulkan.Enumerate.Core (ExtensionProperties(..))
import Vulkan.PhysicalDevice.Struct.Core

import qualified Vulkan.QueueFamily.Core as QueueFamily

#include <vulkan/vulkan.h>

data PhysicalDeviceTag
type PhysicalDevice = Ptr PhysicalDeviceTag

foreign import ccall "vkEnumeratePhysicalDevices" enumerate ::
	Instance -> Ptr #{type uint32_t} -> Ptr PhysicalDevice ->
	IO #{type VkResult}

type ListCFloat = [#{type float}]

struct "SparseProperties" #{size VkPhysicalDeviceSparseProperties}
		#{alignment VkPhysicalDeviceSparseProperties} [
	("residencyStandard2DBlockShape", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceSparseProperties,
			residencyStandard2DBlockShape} |],
		[| #{poke VkPhysicalDeviceSparseProperties,
			residencyStandard2DBlockShape} |]),
	("residencyStandard2DMultisampleBlockShape", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceSparseProperties,
			residencyStandard2DMultisampleBlockShape} |],
		[| #{poke VkPhysicalDeviceSparseProperties,
			residencyStandard2DMultisampleBlockShape} |]),
	("residencyStandard3DBlockShape", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceSparseProperties,
			residencyStandard3DBlockShape} |],
		[| #{poke VkPhysicalDeviceSparseProperties,
			residencyStandard3DBlockShape} |]),
	("residencyAlignedMipSize", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceSparseProperties,
			residencyAlignedMipSize} |],
		[| #{poke VkPhysicalDeviceSparseProperties,
			residencyAlignedMipSize} |]),
	("residencyNonResidentStrict", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceSparseProperties,
			residencyNonResidentStrict} |],
		[| #{poke VkPhysicalDeviceSparseProperties,
			residencyNonResidentStrict} |]) ]
	[''Show, ''Storable]

type ListUint8T = [#{type uint8_t}]

struct "Properties" #{size VkPhysicalDeviceProperties}
		#{alignment VkPhysicalDeviceProperties} [
	("apiVersion", ''#{type uint32_t},
		[| #{peek VkPhysicalDeviceProperties, apiVersion} |],
		[| #{poke VkPhysicalDeviceProperties, apiVersion} |]),
	("driverVersion", ''#{type uint32_t},
		[| #{peek VkPhysicalDeviceProperties, driverVersion} |],
		[| #{poke VkPhysicalDeviceProperties, driverVersion} |]),
	("vendorId", ''#{type uint32_t},
		[| #{peek VkPhysicalDeviceProperties, vendorID} |],
		[| #{poke VkPhysicalDeviceProperties, vendorID} |]),
	("deviceId", ''#{type uint32_t},
		[| #{peek VkPhysicalDeviceProperties, deviceID} |],
		[| #{poke VkPhysicalDeviceProperties, deviceID} |]),
	("deviceType", ''#{type VkPhysicalDeviceType},
		[| #{peek VkPhysicalDeviceProperties, deviceType} |],
		[| #{poke VkPhysicalDeviceProperties, deviceType} |]),
	("deviceName", ''T.Text,
		[| cstringToText
			. #{ptr VkPhysicalDeviceProperties, deviceName} |],
		[| \p -> pokeText vkMaxPhysicalDeviceNameSize
			(#{ptr VkPhysicalDeviceProperties, deviceName} p) |]),
	("pipelineCacheUuid", ''ListUint8T,
		[| peekArray #{const VK_UUID_SIZE}
			. #{ptr VkPhysicalDeviceProperties, pipelineCacheUUID}
			|],
		[| \p -> pokeArray
			(#{ptr VkPhysicalDeviceProperties, pipelineCacheUUID} p)
				. take #{const VK_UUID_SIZE} |]),
	("limits", ''Limits,
		[| #{peek VkPhysicalDeviceProperties, limits} |],
		[| #{poke VkPhysicalDeviceProperties, limits} |]),
	("sparseProperties", ''SparseProperties,
		[| #{peek VkPhysicalDeviceProperties, sparseProperties} |],
		[| #{poke VkPhysicalDeviceProperties, sparseProperties} |]) ]
	[''Show, ''Storable]

vkMaxPhysicalDeviceNameSize :: Integral n => n
vkMaxPhysicalDeviceNameSize = #{const VK_MAX_PHYSICAL_DEVICE_NAME_SIZE}

foreign import ccall "vkGetPhysicalDeviceProperties" getProperties ::
	PhysicalDevice -> Ptr Properties -> IO ()

getCleardFeatures :: IO Features
getCleardFeatures = do
	pf <- calloc
	Features_ <$> newForeignPtr pf (free pf)

foreign import ccall "vkGetPhysicalDeviceFeatures" getFeatures ::
	PhysicalDevice -> Ptr Features -> IO ()

foreign import ccall "vkGetPhysicalDeviceQueueFamilyProperties"
	getQueueFamilyProperties ::
	PhysicalDevice -> Ptr #{type uint32_t} -> Ptr QueueFamily.Properties ->
	IO ()

foreign import ccall "vkGetPhysicalDeviceSurfaceSupportKHR" getSurfaceSupport ::
	PhysicalDevice -> #{type uint32_t} -> Surface -> Ptr #{type VkBool32} ->
	IO #{type VkResult}

foreign import ccall "vkEnumerateDeviceExtensionProperties"
	enumerateExtensionProperties ::
	PhysicalDevice -> CString -> Ptr #{type uint32_t} ->
	Ptr ExtensionProperties -> IO #{type VkResult}
