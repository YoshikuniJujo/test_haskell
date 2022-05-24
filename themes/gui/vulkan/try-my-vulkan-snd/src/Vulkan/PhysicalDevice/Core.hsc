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

import Vulkan.Core (ExtensionProperties(..))
import Vulkan.PhysicalDevice.Struct.Core

import qualified Vulkan.Instance.Core as Instance
import qualified Vulkan.QueueFamily.Core as QueueFamily
import qualified Vulkan.Memory.Core as Memory
import qualified Vulkan.Format.Core as Format

#include <vulkan/vulkan.h>

data PTag
type P = Ptr PTag

foreign import ccall "vkEnumeratePhysicalDevices" enumerate ::
	Instance.I -> Ptr #{type uint32_t} -> Ptr P ->
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
	P -> Ptr Properties -> IO ()

getClearedFeatures :: IO Features
getClearedFeatures = do
	pf <- calloc
	Features_ <$> newForeignPtr pf (free pf)

foreign import ccall "vkGetPhysicalDeviceFeatures" getFeatures ::
	P -> Ptr Features -> IO ()

foreign import ccall "vkGetPhysicalDeviceQueueFamilyProperties"
	getQueueFamilyProperties ::
	P -> Ptr #{type uint32_t} -> Ptr QueueFamily.Properties ->
	IO ()

foreign import ccall "vkEnumerateDeviceExtensionProperties"
	enumerateExtensionProperties ::
	P -> CString -> Ptr #{type uint32_t} ->
	Ptr ExtensionProperties -> IO #{type VkResult}

struct "MemoryProperties" #{size VkPhysicalDeviceMemoryProperties}
		#{alignment VkPhysicalDeviceMemoryProperties} [
	("memoryTypeCount", ''#{type uint32_t},
		[| #{peek VkPhysicalDeviceMemoryProperties, memoryTypeCount} |],
		[| #{poke
			VkPhysicalDeviceMemoryProperties, memoryTypeCount} |]),
	("memoryTypes", ''Memory.ListMType,
		[| \p -> peekArray Memory.maxTypes
			$ #{ptr VkPhysicalDeviceMemoryProperties, memoryTypes} p
			|],
		[| \p -> pokeArray
				(#{ptr VkPhysicalDeviceMemoryProperties,
					memoryTypes} p)
			. take Memory.maxTypes |]),
	("memoryHeapCount", ''#{type uint32_t},
		[| #{peek VkPhysicalDeviceMemoryProperties, memoryHeapCount} |],
		[| #{poke
			VkPhysicalDeviceMemoryProperties, memoryHeapCount} |]),
	("memoryHeaps", ''Memory.ListHeap,
		[| \p -> peekArray Memory.maxHeaps
			$ #{ptr VkPhysicalDeviceMemoryProperties, memoryHeaps} p
			|],
		[| \p -> pokeArray
				(#{ptr VkPhysicalDeviceMemoryProperties,
					memoryHeaps} p) .
			take Memory.maxHeaps |]) ]
	[''Show, ''Storable]

foreign import ccall "vkGetPhysicalDeviceMemoryProperties"
	getMemoryProperties :: P -> Ptr MemoryProperties -> IO ()

foreign import ccall "vkGetPhysicalDeviceFormatProperties"
	getFormatProperties ::
	P -> #{type VkFormat} -> Ptr Format.Properties -> IO ()
