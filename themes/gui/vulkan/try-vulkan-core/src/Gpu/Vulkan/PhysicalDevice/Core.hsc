{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PhysicalDevice.Core (

	-- * ENUMERATE

	enumerate, P,

	-- * PROPERTIES

	getProperties, Properties, pattern Properties,
	propertiesApiVersion, propertiesDriverVersion,
	propertiesVendorId, propertiesDeviceId,
	propertiesDeviceType, propertiesDeviceName,
	propertiesPipelineCacheUuid, propertiesLimits,
	propertiesSparseProperties,

	-- ** SparseProperties

	SparseProperties, pattern SparseProperties,
	sparsePropertiesResidencyStandard2DBlockShape,
	sparsePropertiesResidencyStandard2DMultisampleBlockShape,
	sparsePropertiesResidencyStandard3DBlockShape,
	sparsePropertiesResidencyAlignedMipSize,
	sparsePropertiesResidencyNonResidentStrict,

	-- ** ExtensionProperties

	enumerateExtensionProperties,

	-- ** QqueueFamilyProperties

	getQueueFamilyProperties,

	-- ** MemoryProperties

	getMemoryProperties, MemoryProperties, pattern MemoryProperties,
	memoryPropertiesMemoryTypeCount, memoryPropertiesMemoryTypes,
	memoryPropertiesMemoryHeapCount, memoryPropertiesMemoryHeaps,

	-- ** FormatProperties

	getFormatProperties,

	-- * FEATURES

	-- ** Get Features

	getFeatures, Features, pattern Features, getClearedFeatures,

	-- ** Get Features 2

	getFeatures2, Features2,
	pattern Features2, features2SType, features2PNext, features2Features,

	-- ** ShaderDrawParametersFeatures

	ShaderDrawParametersFeatures, pattern ShaderDrawParametersFeatures,
	shaderDrawParametersFeaturesSType, shaderDrawParametersFeaturesPNext,
	shaderDrawParametersFeaturesShaderDrawParameters

	) where

import Foreign.Ptr
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word
import Data.Int
import Data.Text.Tools

import qualified Data.Text as T

-- import Gpu.Vulkan.Base

import Gpu.Vulkan.Core (ExtensionProperties, FormatProperties)
import Gpu.Vulkan.TypeSynonyms.Core
import Gpu.Vulkan.PhysicalDevice.Struct.Core

import qualified Gpu.Vulkan.Instance.Core as Instance
import qualified Gpu.Vulkan.QueueFamily.Core as QueueFamily
import qualified Gpu.Vulkan.Memory.Core as Memory

#include <vulkan/vulkan.h>

data PTag
type P = Ptr PTag

foreign import ccall "vkEnumeratePhysicalDevices" enumerate ::
	Instance.I -> Ptr #{type uint32_t} -> Ptr P ->
	IO #{type VkResult}

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
		[| \p -> pokeText maxNameSize
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

maxNameSize :: Integral n => n
maxNameSize = #{const VK_MAX_PHYSICAL_DEVICE_NAME_SIZE}

foreign import ccall "vkGetPhysicalDeviceProperties" getProperties ::
	P -> Ptr Properties -> IO ()

getClearedFeatures :: IO Features
getClearedFeatures = do
	pf <- calloc
	Features_ <$> newForeignPtr pf (free pf)

foreign import ccall "vkGetPhysicalDeviceFeatures" getFeatures ::
	P -> Ptr Features -> IO ()

sTypeFeatures2 :: #{type VkStructureType}
sTypeFeatures2 = #{const VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2}

struct "Features2"
	#{size VkPhysicalDeviceFeatures2}
	#{alignment VkPhysicalDeviceFeatures2} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkPhysicalDeviceFeatures2, sType}
			p sTypeFeatures2 |]),
	("pNext", ''PtrVoid,
		[| #{peek VkPhysicalDeviceFeatures2, pNext} |],
		[| #{poke VkPhysicalDeviceFeatures2, pNext} |]),
	("features", ''Features,
		[| peek . #{ptr VkPhysicalDeviceFeatures2, features} |],
		[| poke . #{ptr VkPhysicalDeviceFeatures2, features} |]) ]
	[''Show, ''Storable]

foreign import ccall "vkGetPhysicalDeviceFeatures2" getFeatures2 ::
	P -> Ptr Features2 -> IO ()

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
	P -> #{type VkFormat} -> Ptr FormatProperties -> IO ()

shaderDrawParametersFeaturesType :: #{type VkStructureType}
shaderDrawParametersFeaturesType =
	#{const VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES}

struct "ShaderDrawParametersFeatures"
	#{size VkPhysicalDeviceShaderDrawParametersFeatures}
	#{alignment VkPhysicalDeviceShaderDrawParametersFeatures} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ ->
			#{poke VkPhysicalDeviceShaderDrawParametersFeatures,
				sType} p shaderDrawParametersFeaturesType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkPhysicalDeviceShaderDrawParametersFeatures, pNext} |],
		[| #{poke VkPhysicalDeviceShaderDrawParametersFeatures, pNext} |]),
	("shaderDrawParameters", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceShaderDrawParametersFeatures,
			shaderDrawParameters} |],
		[| #{poke VkPhysicalDeviceShaderDrawParametersFeatures,
			shaderDrawParameters} |]) ]
	[''Show, ''Storable]
