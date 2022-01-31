{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.PhysicalDevice where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Enum
import Foreign.C.Struct
import Control.Monad.Cont
import Data.Bits
import Data.Word
import Data.Int

import Vulkan.Base
import Vulkan.Instance
import Vulkan.Exception
import Vulkan.SampleCountFlagBits

#include <vulkan/vulkan.h>

newtype PhysicalDevice = PhysicalDevice (Ptr PhysicalDevice)
	deriving (Show, Storable)

pattern PhysicalDeviceNullHandle :: PhysicalDevice
pattern PhysicalDeviceNullHandle <- PhysicalDevice NullHandle where
	PhysicalDeviceNullHandle = PhysicalDevice NullHandle

enumeratePhysicalDevices :: Instance -> IO [PhysicalDevice]
enumeratePhysicalDevices (Instance pist) = alloca \pn -> do
	r <- c_vkEnumeratePhysicalDevices pist pn NullPtr
	throwUnlessSuccess r
	n <- peek pn
	allocaArray (fromIntegral n) \ppd -> do
		r' <- c_vkEnumeratePhysicalDevices pist pn ppd
		throwUnlessSuccess r'
--		(PhysicalDevice <$>) <$> peekArray (fromIntegral n) ppd
		peekArray (fromIntegral n) ppd

foreign import ccall "vkEnumeratePhysicalDevices"
	c_vkEnumeratePhysicalDevices ::
	Ptr Instance -> Ptr #{type uint32_t} -> Ptr PhysicalDevice -> IO Result

enum "PhysicalDeviceType" ''#{type VkPhysicalDeviceType} [''Show, ''Storable] [
	("PhysicalDeviceTypeOther", #{const VK_PHYSICAL_DEVICE_TYPE_OTHER}),
	("PhysicalDeviceTypeIntegratedGpu",
		#{const VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU}),
	("PhysicalDeviceTypeDiscreteGpu",
		#{const VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU}),
	("PhysicalDeviceTypeVirtualGpu",
		#{const VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU}),
	("PhysicalDeviceTypeCpu", #{const VK_PHYSICAL_DEVICE_TYPE_CPU}),
	("PhysicalDeviceTypeMaxEnum",
		#{const VK_PHYSICAL_DEVICE_TYPE_MAX_ENUM}) ]

-- VkPhysicalDeviceLimits

type ListUint32T = [#{type uint32_t}]

struct "PhysicalDeviceLimits" #{size VkPhysicalDeviceLimits}
		#{alignment VkPhysicalDeviceLimits} [
	("maxImageDimension1D", ''#{type uint32_t},
		[| #{peek VkPhysicalDeviceLimits, maxImageDimension1D} |],
		[| #{poke VkPhysicalDeviceLimits, maxImageDimension1D} |]),
	("maxImageDimension2D", ''#{type uint32_t},
		[| #{peek VkPhysicalDeviceLimits, maxImageDimension2D} |],
		[| #{poke VkPhysicalDeviceLimits, maxImageDimension2D} |]),
	("maxImageDimension3D", ''#{type uint32_t},
		[| #{peek VkPhysicalDeviceLimits, maxImageDimension3D} |],
		[| #{poke VkPhysicalDeviceLimits, maxImageDimension3D} |]),
	("maxImageDimensionCube", ''#{type uint32_t},
		[| #{peek VkPhysicalDeviceLimits, maxImageDimensionCube} |],
		[| #{poke VkPhysicalDeviceLimits, maxImageDimensionCube} |]),
	("maxImageArrayLayers", ''#{type uint32_t},
		[| #{peek VkPhysicalDeviceLimits, maxImageArrayLayers} |],
		[| #{poke VkPhysicalDeviceLimits, maxImageArrayLayers} |]),
	("maxTexelBufferElements", ''#{type uint32_t},
		[| #{peek VkPhysicalDeviceLimits, maxTexelBufferElements} |],
		[| #{poke VkPhysicalDeviceLimits, maxTexelBufferElements} |]),
	("maxUniformBufferRange", ''#{type uint32_t},
		[| #{peek VkPhysicalDeviceLimits, maxUniformBufferRange} |],
		[| #{poke VkPhysicalDeviceLimits, maxUniformBufferRange} |]),
	("maxStorageBufferRange", ''#{type uint32_t},
		[| #{peek VkPhysicalDeviceLimits, maxStorageBufferRange} |],
		[| #{poke VkPhysicalDeviceLimits, maxStorageBufferRange} |]),
	("maxPushConstantsSize", ''#{type uint32_t},
		[| #{peek VkPhysicalDeviceLimits, maxPushConstantsSize} |],
		[| #{poke VkPhysicalDeviceLimits, maxPushConstantsSize} |]),
	("maxMemoryAllocationCount", ''#{type uint32_t},
		[| #{peek VkPhysicalDeviceLimits, maxMemoryAllocationCount} |],
		[| #{poke VkPhysicalDeviceLimits,
			maxMemoryAllocationCount} |]),
	("maxSamplerAllocationCount", ''#{type uint32_t},
		[| #{peek VkPhysicalDeviceLimits,
			maxSamplerAllocationCount} |],
		[| #{poke VkPhysicalDeviceLimits,
			maxSamplerAllocationCount} |]),
	 ("bufferImageGrannularity", ''#{type VkDeviceSize},
		[| #{peek VkPhysicalDeviceLimits, bufferImageGranularity} |],
		[| #{poke VkPhysicalDeviceLimits, bufferImageGranularity} |]),
	 ("sparseAddressSpaceSize", ''#{type VkDeviceSize},
		[| #{peek VkPhysicalDeviceLimits, sparseAddressSpaceSize} |],
		[| #{poke VkPhysicalDeviceLimits, sparseAddressSpaceSize} |]),
	
	{- maxBoundDescriptorSets, maxPerStageDescriptorSamplers,
	 - maxPerStageDescriptorUniformBuffers,
	 - maxPerStageDescriptorStorageBuffers,
	 - maxPerStageDescriptorSampledImages,
	 - maxPerStageDescriptorStorageImages,
	 - maxPerStageDescriptorInputAttachments, maxPerStageResources,
	 - maxDescriptorSetSamplers -}

	("maxDescriptorSetUniformBuffers", ''#{type uint32_t},
		[| #{peek VkPhysicalDeviceLimits,
			maxDescriptorSetUniformBuffers} |],
		[| #{poke VkPhysicalDeviceLimits,
			maxDescriptorSetUniformBuffers} |]),

	{- maxDescriptorSetUniformBuffersDynamic,
	 - maxDescriptorSetStorageBuffers,
	 - maxDescriptorSetStorageBuffersDynamic,
	 - maxDescriptorSetSampledImages, maxDescriptorSetStorageImages,
	 - maxDescriptorSetInputAttachments -}

	 ("maxVertexInputAttributes", ''#{type uint32_t},
		[| #{peek VkPhysicalDeviceLimits, maxVertexInputAttributes} |],
		[| #{poke VkPhysicalDeviceLimits,
			maxVertexInputAttributes} |]),
	("maxVertexInputBindings", ''#{type uint32_t},
		[| #{peek VkPhysicalDeviceLimits, maxVertexInputBindings} |],
		[| #{poke VkPhysicalDeviceLimits, maxVertexInputBindings} |]),
	("maxVertexInputAttributeOffset", ''#{type uint32_t},
		[| #{peek VkPhysicalDeviceLimits,
			maxVertexInputAttributeOffset} |],
		[| #{poke VkPhysicalDeviceLimits,
			maxVertexInputAttributeOffset} |]),
	("maxVertexInputBindingStride", ''#{type uint32_t},
		[| #{peek VkPhysicalDeviceLimits,
			maxVertexInputBindingStride} |],
		[| #{poke VkPhysicalDeviceLimits,
			maxVertexInputBindingStride} |]),
	("maxVertexOutputComponents", ''#{type uint32_t},
		[| #{peek VkPhysicalDeviceLimits, maxVertexOutputComponents} |],
		[| #{poke VkPhysicalDeviceLimits,
			maxVertexOutputComponents} |]),

	{- maxTessellationGenerationLevel, maxTessellationPatchSize,
	 - maxTessellationControlPerVertexInputComponents -}
	
	("maxTessellationControlPerVertexOutputComponents", ''#{type uint32_t},
		[| #{peek VkPhysicalDeviceLimits,
			maxTessellationControlPerVertexOutputComponents} |],
		[| #{poke VkPhysicalDeviceLimits,
			maxTessellationControlPerVertexOutputComponents} |]),

	{- maxTessellationControlPerPatchOutputCompoonents,
	 - maxTessellationControlTotalOutputComponents,
	 - maxTessellationEvaluationInputComponents,
	 - maxTessellationEvaluationOutputComponents,
	 - maxGeometryShaderInvocations -}

	("maxGeometryInputComponents", ''#{type uint32_t},
		[| #{peek VkPhysicalDeviceLimits,
			maxGeometryInputComponents} |],
		[| #{poke VkPhysicalDeviceLimits,
			maxGeometryInputComponents} |]),

	{- maxGeometryOutputComponents, maxGeometryOutputVertices,
	 - maxGeometryTotalOutputComponents, maxFragmentInputComponents,
	 - maxFragmentOutputAttachments, maxFragmentDualSrcAttachments,
	 - maxFragmentCombinedOutputResources, maxComputeSharedMemorySize -}

	 ("maxComputeWorkGroupCount", ''ListUint32T,
		[| peekArray 3 . #{ptr VkPhysicalDeviceLimits,
			maxComputeWorkGroupCount} |],
		[| pokeArray . #{ptr VkPhysicalDeviceLimits,
			maxComputeWorkGroupCount} |]),

	{- maxComputeWorkGroupInvocations, maxComputeWorkGroupSize[3],
	 - subPixelPrecisionBits, subTexelPrecisionBits, mipmapPrecisionBits,
	 - maxDrawIndexedIndexValue, maxDrawIndirectCount -}

	("maxSamplerLodBias", ''#{type float},
		[| #{peek VkPhysicalDeviceLimits, maxSamplerLodBias} |],
		[| #{poke VkPhysicalDeviceLimits, maxSamplerLodBias} |]),
	("maxSamplerAnisotropy", ''#{type float},
		[| #{peek VkPhysicalDeviceLimits, maxSamplerAnisotropy} |],
		[| #{poke VkPhysicalDeviceLimits, maxSamplerAnisotropy} |]),

	{- maxViewports, maxViewportDimensions[2], viewportBoundsRange[2],
	 - viewPortSubPixelBits, minMemoryMapAlignment,
	 - minTexelBufferOffsetAlignment, minUniformBufferOffsetAlignment,
	 - minStorageBufferOffsetAlignment -}

	("minTexelOffset", ''#{type int32_t},
		[| #{peek VkPhysicalDeviceLimits, minTexelOffset} |],
		[| #{poke VkPhysicalDeviceLimits, minTexelOffset} |]),
	("maxTexelOffset", ''#{type uint32_t},
		[| #{peek VkPhysicalDeviceLimits, maxTexelOffset} |],
		[| #{poke VkPhysicalDeviceLimits, maxTexelOffset} |]),

	{- minTexelGatherOffset, maxTexelGatherOffset, minInterpolationOffset,
	 - maxInterpolationOffset -}

	("subPixelInterpolationOffsetBits", ''#{type uint32_t},
		[| #{peek VkPhysicalDeviceLimits,
			subPixelInterpolationOffsetBits} |],
		[| #{poke VkPhysicalDeviceLimits,
			subPixelInterpolationOffsetBits} |]),

	{- maxFramebufferWidth, maxFramebufferHeight, maxFramebufferLayers -}

	("frameBufferColorSampleCounts", ''SampleCountFlags,
		[| #{peek VkPhysicalDeviceLimits,
			framebufferColorSampleCounts} |],
		[| #{poke VkPhysicalDeviceLimits,
			framebufferColorSampleCounts} |]),
	("frameBufferDepthSampleCounts", ''SampleCountFlags,
		[| #{peek VkPhysicalDeviceLimits,
			framebufferDepthSampleCounts} |],
		[| #{poke VkPhysicalDeviceLimits,
			framebufferDepthSampleCounts} |]),
	("framebufferStencilSampleCounts", ''SampleCountFlags,
		[| #{peek VkPhysicalDeviceLimits,
			framebufferStencilSampleCounts} |],
		[| #{poke VkPhysicalDeviceLimits,
			framebufferStencilSampleCounts} |]),
	("framebufferNoAttachmentsSampleCounts", ''SampleCountFlags,
		[| #{peek VkPhysicalDeviceLimits,
			framebufferNoAttachmentsSampleCounts} |],
		[| #{poke VkPhysicalDeviceLimits,
			framebufferNoAttachmentsSampleCounts} |]),
	("maxColorAttachments", ''#{type uint32_t},
		[| #{peek VkPhysicalDeviceLimits, maxColorAttachments} |],
		[| #{poke VkPhysicalDeviceLimits, maxColorAttachments} |]),
	("sampledImageColorSampleCounts", ''SampleCountFlags,
		[| #{peek VkPhysicalDeviceLimits,
			sampledImageColorSampleCounts} |],
		[| #{poke VkPhysicalDeviceLimits,
			sampledImageColorSampleCounts} |]),
	("sampledImageIntegerSampleCounts", ''SampleCountFlags,
		[| #{peek VkPhysicalDeviceLimits,
			sampledImageIntegerSampleCounts} |],
		[| #{poke VkPhysicalDeviceLimits,
			sampledImageIntegerSampleCounts} |]),
	("sampledImageDepthSampleCounts", ''SampleCountFlags,
		[| #{peek VkPhysicalDeviceLimits,
			sampledImageDepthSampleCounts} |],
		[| #{poke VkPhysicalDeviceLimits,
			sampledImageDepthSampleCounts} |]),
	("sampledImageStencilSampleCounts", ''SampleCountFlags,
		[| #{peek VkPhysicalDeviceLimits,
			sampledImageStencilSampleCounts} |],
		[| #{poke VkPhysicalDeviceLimits,
			sampledImageStencilSampleCounts} |]),

	{- storageImageSampleCounts,
	 - maxSampleMaskWords, timestampComputeAndGraphics,
	 - timestampPeriod, maxClipDistances, maxCullDistances,
	 - maxCombinedClipAndCullDistances -}

	("discreteQueuePriorities", ''#{type uint32_t},
		[| #{peek VkPhysicalDeviceLimits, discreteQueuePriorities} |],
		[| #{poke VkPhysicalDeviceLimits, discreteQueuePriorities} |]),
	("pointSizeRange", ''ListCFloat,
		[| peekArray 2
			. #{ptr VkPhysicalDeviceLimits, pointSizeRange} |],
		[| \p -> pokeArray
				(#{ptr VkPhysicalDeviceLimits, pointSizeRange}
				p)
			. take 2 |]),
	("lineWidthRange", ''ListCFloat,
		[| peekArray 2
			. #{ptr VkPhysicalDeviceLimits, lineWidthRange} |],
		[| \p -> pokeArray
				(#{ptr VkPhysicalDeviceLimits, lineWidthRange}
				p)
			. take 2 |]),
	("pointSizeGranularity", ''#{type float},
		[| #{peek VkPhysicalDeviceLimits, pointSizeGranularity} |],
		[| #{poke VkPhysicalDeviceLimits, pointSizeGranularity} |]),
	("lineWidthGranularity", ''#{type float},
		[| #{peek VkPhysicalDeviceLimits, lineWidthGranularity} |],
		[| #{poke VkPhysicalDeviceLimits, lineWidthGranularity} |])

	{- strictLines, standardSampleLocations,
	 - optimalBufferCopyOffsetAlignment, optimalBufferCopyRowPitchAlignment,
	 - nonCoherentAtomSize -}
	]
	[''Show, ''Storable]

-- VkPhysicalDeviceSparseProperties

struct "PhysicalDeviceSparseProperties" #{size VkPhysicalDeviceSparseProperties}
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
	("residensyStandard3DBlockShape", ''#{type VkBool32},
		[| #{peek VkPhysicalDeviceSparseProperties,
			residencyStandard3DBlockShape} |],
		[| #{poke VkPhysicalDeviceSparseProperties,
			residencyStandard3DBlockShape} |]),
	("residencyAlienedMipSize", ''#{type VkBool32},
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

-- VkPhysicalDeviceProperties

type ListUint8T = [#{type uint8_t}]

struct "PhysicalDeviceProperties" #{size VkPhysicalDeviceProperties}
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
	("deviceType", ''PhysicalDeviceType,
		[| #{peek VkPhysicalDeviceProperties, deviceType} |],
		[| #{poke VkPhysicalDeviceProperties, deviceType} |]),
	("deviceName", ''String,
		[| peekCString
			. #{ptr VkPhysicalDeviceProperties, deviceName} |],
		[| \p -> pokeCString
			(#{ptr VkPhysicalDeviceProperties, deviceName} p)
				. take (vkMaxPhysicalDeviceNameSize - 1) |]),
	("pipelineCacheUuid", ''ListUint8T,
		[| peekArray #{const VK_UUID_SIZE}
			. #{ptr VkPhysicalDeviceProperties, pipelineCacheUUID}
			|],
		[| \p -> pokeArray
			(#{ptr VkPhysicalDeviceProperties, pipelineCacheUUID} p)
				. take #{const VK_UUID_SIZE} |]),
	("limits", ''PhysicalDeviceLimits,
		[| #{peek VkPhysicalDeviceProperties, limits} |],
		[| #{poke VkPhysicalDeviceProperties, limits} |]),
	("sparseProperties", ''PhysicalDeviceSparseProperties,
		[| #{peek VkPhysicalDeviceProperties, sparseProperties} |],
		[| #{poke VkPhysicalDeviceProperties, sparseProperties} |]) ]
	[''Show, ''Storable]

vkMaxPhysicalDeviceNameSize :: Integral n => n
vkMaxPhysicalDeviceNameSize = #{const VK_MAX_PHYSICAL_DEVICE_NAME_SIZE}

getPhysicalDeviceProperties :: PhysicalDevice -> IO PhysicalDeviceProperties
getPhysicalDeviceProperties (PhysicalDevice ppd) = alloca \ppdp -> do
	c_vkGetPhysicalDeviceProperties ppd ppdp
	peek ppdp

foreign import ccall "vkGetPhysicalDeviceProperties"
	c_vkGetPhysicalDeviceProperties ::
	Ptr PhysicalDevice -> Ptr PhysicalDeviceProperties -> IO ()

struct "PhysicalDeviceFeatures" #{size VkPhysicalDeviceFeatures}
		#{alignment VkPhysicalDeviceFeatures} [
	("robutBufferAccess", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, robustBufferAccess} |],
		[| #{poke VkPhysicalDeviceFeatures, robustBufferAccess} |]),
	("fullDrawIndexUint32", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, fullDrawIndexUint32} |],
		[| #{poke VkPhysicalDeviceFeatures, fullDrawIndexUint32} |]),
	("imageCubeArray", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, imageCubeArray} |],
		[| #{poke VkPhysicalDeviceFeatures, imageCubeArray} |]),
	("independentBlend", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, independentBlend} |],
		[| #{poke VkPhysicalDeviceFeatures, independentBlend} |]),
	("geometryShader", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, geometryShader} |],
		[| #{poke VkPhysicalDeviceFeatures, geometryShader} |]),
	("tessellationShader", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, tessellationShader} |],
		[| #{poke VkPhysicalDeviceFeatures, tessellationShader} |]),
	("sampleRateShading", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, sampleRateShading} |],
		[| #{poke VkPhysicalDeviceFeatures, sampleRateShading} |]),
	("dualSrcBlend", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, dualSrcBlend} |],
		[| #{poke VkPhysicalDeviceFeatures, dualSrcBlend} |]),
	("logicOp", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, logicOp} |],
		[| #{poke VkPhysicalDeviceFeatures, logicOp} |]),
	("multiDrawIndirect", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, multiDrawIndirect} |],
		[| #{poke VkPhysicalDeviceFeatures, multiDrawIndirect} |]),
	("drawIndirectFirstInstance", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures,
			drawIndirectFirstInstance} |],
		[| #{poke VkPhysicalDeviceFeatures,
			drawIndirectFirstInstance} |]),
	("depthClamp", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, depthClamp} |],
		[| #{poke VkPhysicalDeviceFeatures, depthClamp} |]),
	("depthBiasClamp", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, depthBiasClamp} |],
		[| #{poke VkPhysicalDeviceFeatures, depthBiasClamp} |]),
	("fillModeNonSolid", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, fillModeNonSolid} |],
		[| #{poke VkPhysicalDeviceFeatures, fillModeNonSolid} |]),
	("depthBounds", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, depthBounds} |],
		[| #{poke VkPhysicalDeviceFeatures, depthBounds} |]),
	("wideLines", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, wideLines} |],
		[| #{poke VkPhysicalDeviceFeatures, wideLines} |]),
	("largePoints", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, largePoints} |],
		[| #{poke VkPhysicalDeviceFeatures, largePoints} |]),
	("alphaToOne", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, alphaToOne} |],
		[| #{poke VkPhysicalDeviceFeatures, alphaToOne} |]),
	("multiViewport", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, multiViewport} |],
		[| #{poke VkPhysicalDeviceFeatures, multiViewport} |]),
	("samplerAnisotropy", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, samplerAnisotropy} |],
		[| #{poke VkPhysicalDeviceFeatures, samplerAnisotropy} |]),
	("textureCompressionEtc2", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, textureCompressionETC2} |],
		[| #{poke VkPhysicalDeviceFeatures, textureCompressionETC2} |]),
	("textureCompressionAstcLdr", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures,
			textureCompressionASTC_LDR} |],
		[| #{poke VkPhysicalDeviceFeatures,
			textureCompressionASTC_LDR} |]),
	("textureCompressionBc", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, textureCompressionBC} |],
		[| #{poke VkPhysicalDeviceFeatures, textureCompressionBC} |]),
	("occlusionQueryPrecise", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, occlusionQueryPrecise} |],
		[| #{poke VkPhysicalDeviceFeatures, occlusionQueryPrecise} |]),
	("pipelineStatisticsQuery", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, pipelineStatisticsQuery} |],
		[| #{poke VkPhysicalDeviceFeatures,
			pipelineStatisticsQuery} |]),
	("vertexPipelineStoresAndAtomics", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures,
			vertexPipelineStoresAndAtomics} |],
		[| #{poke VkPhysicalDeviceFeatures,
			vertexPipelineStoresAndAtomics} |]),
	("fragmentStoresAndAtomics", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures,
			fragmentStoresAndAtomics} |],
		[| #{poke VkPhysicalDeviceFeatures,
			fragmentStoresAndAtomics} |]),
	("shaderTessellationAndGeometryPointSize", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures,
			shaderTessellationAndGeometryPointSize} |],
		[| #{poke VkPhysicalDeviceFeatures,
			shaderTessellationAndGeometryPointSize} |]),
	("shaderImageGatherExtended", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures,
			shaderImageGatherExtended} |],
		[| #{poke VkPhysicalDeviceFeatures,
			shaderImageGatherExtended} |]),
	("shaderStorageImageExtendedFormats", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures,
			shaderStorageImageExtendedFormats} |],
		[| #{poke VkPhysicalDeviceFeatures,
			shaderStorageImageExtendedFormats} |]),
	("shaderStorageImageMultisample", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures,
			shaderStorageImageMultisample} |],
		[| #{poke VkPhysicalDeviceFeatures,
			shaderStorageImageMultisample} |]),
	("shaderStorageImageReadWithoutFormat", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures,
			shaderStorageImageReadWithoutFormat} |],
		[| #{poke VkPhysicalDeviceFeatures,
			shaderStorageImageReadWithoutFormat} |]),
	("shaderStorageImageWriteWithoutFormat", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures,
			shaderStorageImageWriteWithoutFormat} |],
		[| #{poke VkPhysicalDeviceFeatures,
			shaderStorageImageWriteWithoutFormat} |]),
	("shaderUniformBufferArrayDynamicIndexing", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures,
			shaderUniformBufferArrayDynamicIndexing} |],
		[| #{poke VkPhysicalDeviceFeatures,
			shaderUniformBufferArrayDynamicIndexing} |]),
	("shaderSampledImageArrayDynamicIndexing", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures,
			shaderSampledImageArrayDynamicIndexing} |],
		[| #{poke VkPhysicalDeviceFeatures,
			shaderSampledImageArrayDynamicIndexing} |]),
	("shaderStorageBufferArrayDynamicIndexing", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures,
			shaderStorageBufferArrayDynamicIndexing} |],
		[| #{poke VkPhysicalDeviceFeatures,
			shaderStorageBufferArrayDynamicIndexing} |]),
	("shaderStorageImageArrayDynamicIndexing", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures,
			shaderStorageImageArrayDynamicIndexing} |],
		[| #{poke VkPhysicalDeviceFeatures,
			shaderStorageImageArrayDynamicIndexing} |]),
	("shaderClipDistance", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, shaderClipDistance} |],
		[| #{poke VkPhysicalDeviceFeatures, shaderClipDistance} |]),
	("shaderCullDistance", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, shaderCullDistance} |],
		[| #{poke VkPhysicalDeviceFeatures, shaderCullDistance} |]),
	("shaderFloat64", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, shaderFloat64} |],
		[| #{poke VkPhysicalDeviceFeatures, shaderFloat64} |]),
	("shaderInt64", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, shaderInt64} |],
		[| #{poke VkPhysicalDeviceFeatures, shaderInt64} |]),
	("shaderInt16", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, shaderInt16} |],
		[| #{poke VkPhysicalDeviceFeatures, shaderInt16} |]),
	("shaderResourceResidency", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, shaderResourceResidency} |],
		[| #{poke VkPhysicalDeviceFeatures,
			shaderResourceResidency} |]),
	("shaderResourceMinLod", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, shaderResourceMinLod} |],
		[| #{poke VkPhysicalDeviceFeatures, shaderResourceMinLod} |]),
	("sparseBinding", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, sparseBinding} |],
		[| #{poke VkPhysicalDeviceFeatures, sparseBinding} |]),
	("sparseResidencyBuffer", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, sparseResidencyBuffer} |],
		[| #{poke VkPhysicalDeviceFeatures, sparseResidencyBuffer} |]),
	("sparseResidencyImage2D", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, sparseResidencyImage2D} |],
		[| #{poke VkPhysicalDeviceFeatures, sparseResidencyImage2D} |]),
	("sparseResidencyImage3D", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, sparseResidencyImage3D} |],
		[| #{poke VkPhysicalDeviceFeatures, sparseResidencyImage3D} |]),
	("sparseResidency2Samples", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, sparseResidency2Samples} |],
		[| #{poke VkPhysicalDeviceFeatures,
			sparseResidency2Samples} |]),
	("sparseResidency4Samples", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, sparseResidency4Samples} |],
		[| #{poke VkPhysicalDeviceFeatures,
			sparseResidency4Samples} |]),
	("sparseResidency8Samples", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, sparseResidency8Samples} |],
		[| #{poke VkPhysicalDeviceFeatures,
			sparseResidency8Samples} |]),
	("sparseResidency16Samples", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures,
			sparseResidency16Samples} |],
		[| #{poke VkPhysicalDeviceFeatures,
			sparseResidency16Samples} |]),
	("sparseResidencyAliased", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, sparseResidencyAliased} |],
		[| #{poke VkPhysicalDeviceFeatures, sparseResidencyAliased} |]),
	("variableMultisampleRate", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, variableMultisampleRate} |],
		[| #{poke VkPhysicalDeviceFeatures,
			variableMultisampleRate} |]),
	("inheritedQueries", ''Bool32,
		[| #{peek VkPhysicalDeviceFeatures, inheritedQueries} |],
		[| #{poke VkPhysicalDeviceFeatures, inheritedQueries} |]) ]
	[''Show, ''Storable]

getPhysicalDeviceFeatures :: PhysicalDevice -> IO PhysicalDeviceFeatures
getPhysicalDeviceFeatures (PhysicalDevice ppd) = alloca \ppdf -> do
	c_vkGetPhysicalDeviceFeatures ppd ppdf
	peek ppdf

foreign import ccall "vkGetPhysicalDeviceFeatures"
	c_vkGetPhysicalDeviceFeatures ::
	Ptr PhysicalDevice -> Ptr PhysicalDeviceFeatures -> IO ()

enum "QueueFlagBits" ''#{type VkQueueFlagBits}
		[''Show, ''Eq, ''Storable, ''Bits] [
	("QueueGraphicsBit", #{const VK_QUEUE_GRAPHICS_BIT}),
	("QueueComputeBit", #{const VK_QUEUE_COMPUTE_BIT}),
	("QueueTransferBit", #{const VK_QUEUE_TRANSFER_BIT}),
	("QueueSparseBindingBit", #{const VK_QUEUE_SPARSE_BINDING_BIT}),
	("QueueProtectedBit", #{const VK_QUEUE_PROTECTED_BIT}),
	#ifdef VK_ENABLE_BETA_EXTENSIONS
	("QueueVideoDecodeBitKhr", #{const VK_QUEUE_VIDEO_DECODE_BIT_KHR}),
	("QueueVideoEncodeBitKhr", #{const VK_QUEUE_VIDEO_ENCODE_BIT_KHR}),
	#endif
	("QueueFlagBitsMaxEnum", #{const VK_QUEUE_FLAG_BITS_MAX_ENUM}) ]

type QueueFlags = QueueFlagBits

struct "QueueFamilyProperties" #{size VkQueueFamilyProperties}
		#{alignment VkQueueFamilyProperties} [
	("queueFlags", ''QueueFlags,
		[| #{peek VkQueueFamilyProperties, queueFlags} |],
		[| #{poke VkQueueFamilyProperties, queueFlags} |]),
	("queueCount", ''#{type uint32_t},
		[| #{peek VkQueueFamilyProperties, queueCount} |],
		[| #{poke VkQueueFamilyProperties, queueCount} |]),
	("timestampValidBits", ''#{type uint32_t},
		[| #{peek VkQueueFamilyProperties, timestampValidBits} |],
		[| #{poke VkQueueFamilyProperties, timestampValidBits} |]),
	("minImageTransferGranularity", ''Extent3D,
		[| #{peek VkQueueFamilyProperties,
			minImageTransferGranularity} |],
		[| #{poke VkQueueFamilyProperties,
			minImageTransferGranularity} |]) ]
	[''Show, ''Storable]

getPhysicalDeviceQueueFamilyProperties ::
	PhysicalDevice -> IO [QueueFamilyProperties]
getPhysicalDeviceQueueFamilyProperties pd = alloca \pn -> do
	c_vkGetPhysicalDeviceQueueFamilyProperties pd pn nullPtr
	n <- peek pn
	allocaArray (fromIntegral n) \pProps -> do
		c_vkGetPhysicalDeviceQueueFamilyProperties pd pn pProps
		peekArray (fromIntegral n) pProps

foreign import ccall "vkGetPhysicalDeviceQueueFamilyProperties"
	c_vkGetPhysicalDeviceQueueFamilyProperties ::
	PhysicalDevice -> Ptr #{type uint32_t} -> Ptr QueueFamilyProperties ->
	IO ()

enumerateDeviceExtensionProperties ::
	PhysicalDevice -> Maybe String -> IO [ExtensionProperties]
enumerateDeviceExtensionProperties phdv mln = ($ pure) $ runContT do
	cln <- case mln of
		Nothing -> pure NullPtr
		Just ln -> ContT $ withCString ln
	pn <- ContT alloca
	n <- lift do
		r <- c_vkEnumerateDeviceExtensionProperties phdv cln pn NullPtr
		throwUnlessSuccess r
		peek pn
	pprps <- ContT . allocaArray $ fromIntegral n
	lift do	r <- c_vkEnumerateDeviceExtensionProperties phdv cln pn pprps
		throwUnlessSuccess r
		peekArray (fromIntegral n) pprps

foreign import ccall "vkEnumerateDeviceExtensionProperties"
	c_vkEnumerateDeviceExtensionProperties ::
	PhysicalDevice -> CString ->
	Ptr #{type uint32_t} -> Ptr ExtensionProperties -> IO Result
