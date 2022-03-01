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

import qualified Vulkan.Queue.Family as Queue.Family

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

struct "Features" #{size VkPhysicalDeviceFeatures}
		#{alignment VkPhysicalDeviceFeatures} [
	("robutBufferAccess", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, robustBufferAccess} |],
		[| #{poke VkPhysicalDeviceFeatures, robustBufferAccess} |]),
	("fullDrawIndexUint32", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, fullDrawIndexUint32} |],
		[| #{poke VkPhysicalDeviceFeatures, fullDrawIndexUint32} |]),
	("imageCubeArray", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, imageCubeArray} |],
		[| #{poke VkPhysicalDeviceFeatures, imageCubeArray} |]),
	("independentBlend", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, independentBlend} |],
		[| #{poke VkPhysicalDeviceFeatures, independentBlend} |]),
	("geometryShader", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, geometryShader} |],
		[| #{poke VkPhysicalDeviceFeatures, geometryShader} |]),
	("tessellationShader", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, tessellationShader} |],
		[| #{poke VkPhysicalDeviceFeatures, tessellationShader} |]),
	("sampleRateShading", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, sampleRateShading} |],
		[| #{poke VkPhysicalDeviceFeatures, sampleRateShading} |]),
	("dualSrcBlend", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, dualSrcBlend} |],
		[| #{poke VkPhysicalDeviceFeatures, dualSrcBlend} |]),
	("logicOp", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, logicOp} |],
		[| #{poke VkPhysicalDeviceFeatures, logicOp} |]),
	("multiDrawIndirect", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, multiDrawIndirect} |],
		[| #{poke VkPhysicalDeviceFeatures, multiDrawIndirect} |]),
	("drawIndirectFirstInstance", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures,
			drawIndirectFirstInstance} |],
		[| #{poke VkPhysicalDeviceFeatures,
			drawIndirectFirstInstance} |]),
	("depthClamp", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, depthClamp} |],
		[| #{poke VkPhysicalDeviceFeatures, depthClamp} |]),
	("depthBiasClamp", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, depthBiasClamp} |],
		[| #{poke VkPhysicalDeviceFeatures, depthBiasClamp} |]),
	("fillModeNonSolid", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, fillModeNonSolid} |],
		[| #{poke VkPhysicalDeviceFeatures, fillModeNonSolid} |]),
	("depthBounds", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, depthBounds} |],
		[| #{poke VkPhysicalDeviceFeatures, depthBounds} |]),
	("wideLines", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, wideLines} |],
		[| #{poke VkPhysicalDeviceFeatures, wideLines} |]),
	("largePoints", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, largePoints} |],
		[| #{poke VkPhysicalDeviceFeatures, largePoints} |]),
	("alphaToOne", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, alphaToOne} |],
		[| #{poke VkPhysicalDeviceFeatures, alphaToOne} |]),
	("multiViewport", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, multiViewport} |],
		[| #{poke VkPhysicalDeviceFeatures, multiViewport} |]),
	("samplerAnisotropy", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, samplerAnisotropy} |],
		[| #{poke VkPhysicalDeviceFeatures, samplerAnisotropy} |]),
	("textureCompressionEtc2", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, textureCompressionETC2} |],
		[| #{poke VkPhysicalDeviceFeatures, textureCompressionETC2} |]),
	("textureCompressionAstcLdr", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures,
			textureCompressionASTC_LDR} |],
		[| #{poke VkPhysicalDeviceFeatures,
			textureCompressionASTC_LDR} |]),
	("textureCompressionBc", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, textureCompressionBC} |],
		[| #{poke VkPhysicalDeviceFeatures, textureCompressionBC} |]),
	("occlusionQueryPrecise", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, occlusionQueryPrecise} |],
		[| #{poke VkPhysicalDeviceFeatures, occlusionQueryPrecise} |]),
	("pipelineStatisticsQuery", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, pipelineStatisticsQuery} |],
		[| #{poke VkPhysicalDeviceFeatures,
			pipelineStatisticsQuery} |]),
	("vertexPipelineStoresAndAtomics", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures,
			vertexPipelineStoresAndAtomics} |],
		[| #{poke VkPhysicalDeviceFeatures,
			vertexPipelineStoresAndAtomics} |]),
	("fragmentStoresAndAtomics", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures,
			fragmentStoresAndAtomics} |],
		[| #{poke VkPhysicalDeviceFeatures,
			fragmentStoresAndAtomics} |]),
	("shaderTessellationAndGeometryPointSize", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures,
			shaderTessellationAndGeometryPointSize} |],
		[| #{poke VkPhysicalDeviceFeatures,
			shaderTessellationAndGeometryPointSize} |]),
	("shaderImageGatherExtended", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures,
			shaderImageGatherExtended} |],
		[| #{poke VkPhysicalDeviceFeatures,
			shaderImageGatherExtended} |]),
	("shaderStorageImageExtendedFormats", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures,
			shaderStorageImageExtendedFormats} |],
		[| #{poke VkPhysicalDeviceFeatures,
			shaderStorageImageExtendedFormats} |]),
	("shaderStorageImageMultisample", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures,
			shaderStorageImageMultisample} |],
		[| #{poke VkPhysicalDeviceFeatures,
			shaderStorageImageMultisample} |]),
	("shaderStorageImageReadWithoutFormat", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures,
			shaderStorageImageReadWithoutFormat} |],
		[| #{poke VkPhysicalDeviceFeatures,
			shaderStorageImageReadWithoutFormat} |]),
	("shaderStorageImageWriteWithoutFormat", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures,
			shaderStorageImageWriteWithoutFormat} |],
		[| #{poke VkPhysicalDeviceFeatures,
			shaderStorageImageWriteWithoutFormat} |]),
	("shaderUniformBufferArrayDynamicIndexing", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures,
			shaderUniformBufferArrayDynamicIndexing} |],
		[| #{poke VkPhysicalDeviceFeatures,
			shaderUniformBufferArrayDynamicIndexing} |]),
	("shaderSampledImageArrayDynamicIndexing", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures,
			shaderSampledImageArrayDynamicIndexing} |],
		[| #{poke VkPhysicalDeviceFeatures,
			shaderSampledImageArrayDynamicIndexing} |]),
	("shaderStorageBufferArrayDynamicIndexing", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures,
			shaderStorageBufferArrayDynamicIndexing} |],
		[| #{poke VkPhysicalDeviceFeatures,
			shaderStorageBufferArrayDynamicIndexing} |]),
	("shaderStorageImageArrayDynamicIndexing", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures,
			shaderStorageImageArrayDynamicIndexing} |],
		[| #{poke VkPhysicalDeviceFeatures,
			shaderStorageImageArrayDynamicIndexing} |]),
	("shaderClipDistance", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, shaderClipDistance} |],
		[| #{poke VkPhysicalDeviceFeatures, shaderClipDistance} |]),
	("shaderCullDistance", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, shaderCullDistance} |],
		[| #{poke VkPhysicalDeviceFeatures, shaderCullDistance} |]),
	("shaderFloat64", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, shaderFloat64} |],
		[| #{poke VkPhysicalDeviceFeatures, shaderFloat64} |]),
	("shaderInt64", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, shaderInt64} |],
		[| #{poke VkPhysicalDeviceFeatures, shaderInt64} |]),
	("shaderInt16", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, shaderInt16} |],
		[| #{poke VkPhysicalDeviceFeatures, shaderInt16} |]),
	("shaderResourceResidency", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, shaderResourceResidency} |],
		[| #{poke VkPhysicalDeviceFeatures,
			shaderResourceResidency} |]),
	("shaderResourceMinLod", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, shaderResourceMinLod} |],
		[| #{poke VkPhysicalDeviceFeatures, shaderResourceMinLod} |]),
	("sparseBinding", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, sparseBinding} |],
		[| #{poke VkPhysicalDeviceFeatures, sparseBinding} |]),
	("sparseResidencyBuffer", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, sparseResidencyBuffer} |],
		[| #{poke VkPhysicalDeviceFeatures, sparseResidencyBuffer} |]),
	("sparseResidencyImage2D", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, sparseResidencyImage2D} |],
		[| #{poke VkPhysicalDeviceFeatures, sparseResidencyImage2D} |]),
	("sparseResidencyImage3D", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, sparseResidencyImage3D} |],
		[| #{poke VkPhysicalDeviceFeatures, sparseResidencyImage3D} |]),
	("sparseResidency2Samples", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, sparseResidency2Samples} |],
		[| #{poke VkPhysicalDeviceFeatures,
			sparseResidency2Samples} |]),
	("sparseResidency4Samples", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, sparseResidency4Samples} |],
		[| #{poke VkPhysicalDeviceFeatures,
			sparseResidency4Samples} |]),
	("sparseResidency8Samples", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, sparseResidency8Samples} |],
		[| #{poke VkPhysicalDeviceFeatures,
			sparseResidency8Samples} |]),
	("sparseResidency16Samples", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures,
			sparseResidency16Samples} |],
		[| #{poke VkPhysicalDeviceFeatures,
			sparseResidency16Samples} |]),
	("sparseResidencyAliased", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, sparseResidencyAliased} |],
		[| #{poke VkPhysicalDeviceFeatures, sparseResidencyAliased} |]),
	("variableMultisampleRate", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, variableMultisampleRate} |],
		[| #{poke VkPhysicalDeviceFeatures,
			variableMultisampleRate} |]),
	("inheritedQueries", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceFeatures, inheritedQueries} |],
		[| #{poke VkPhysicalDeviceFeatures, inheritedQueries} |]) ]
	[''Show, ''Storable]

type PtrFeatures = Ptr Features

getCleardFeatures :: IO Features
getCleardFeatures = do
	pf <- calloc
	Features_ <$> newForeignPtr pf (free pf)

foreign import ccall "vkGetPhysicalDeviceFeatures" getFeatures ::
	PhysicalDevice -> Ptr Features -> IO ()

foreign import ccall "vkGetPhysicalDeviceQueueFamilyProperties"
	getQueueFamilyProperties ::
	PhysicalDevice -> Ptr #{type uint32_t} -> Ptr Queue.Family.Properties ->
	IO ()

foreign import ccall "vkGetPhysicalDeviceSurfaceSupportKHR" getSurfaceSupport ::
	PhysicalDevice -> #{type uint32_t} -> Surface -> Ptr #{type VkBool32} ->
	IO #{type VkResult}

foreign import ccall "vkEnumerateDeviceExtensionProperties"
	enumerateExtensionProperties ::
	PhysicalDevice -> CString -> Ptr #{type uint32_t} ->
	Ptr ExtensionProperties -> IO #{type VkResult}
