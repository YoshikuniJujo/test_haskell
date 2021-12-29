{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan (
	module Vulkan, I.makeApiVersion, I.apiVersion1_0,
	pattern I.InstanceCreateFlagsZero,
	I.FnAllocationFunction, I.FnReallocationFunction, I.FnFreeFunction,
	I.FnInternalAllocationNotification, I.FnInternalFreeNotification ) where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Enum
import Foreign.C.Struct
import Data.Word

import Vulkan.Exception
import Vulkan.Instance
import Vulkan.Base
import Vulkan.AllocationCallbacks

import qualified Vulkan.Internal as I
import qualified Vulkan.AllocationCallbacks.Internal as I

#include <vulkan/vulkan.h>

struct "ExtensionProperties" #{size VkExtensionProperties}
		#{alignment VkExtensionProperties} [
	("extensionName", ''String,
		[| peekCString . #{ptr VkExtensionProperties, extensionName} |],
		[| \p s -> pokeCStringLen
			#{const VK_MAX_EXTENSION_NAME_SIZE}
			(#{ptr VkExtensionProperties, extensionName} p) s |]),
	("specVersion", ''#{type uint32_t}, [| #{peek VkExtensionProperties, specVersion} |],
		[| #{poke VkExtensionProperties, specVersion} |])
	]
	[''Show, ''Read, ''Eq, ''Storable]

enumerateInstanceExtensionProperties :: Maybe String -> IO [ExtensionProperties]
enumerateInstanceExtensionProperties =
	flip withMaybeCString \cs -> alloca \pn -> do
		r <- c_vkEnumerateInstanceExtensionProperties cs pn nullPtr
		throwUnlessSuccess r
		n <- peek pn
		allocaArray (fromIntegral n) \pProps -> do
			r' <- c_vkEnumerateInstanceExtensionProperties cs pn pProps
			throwUnlessSuccess r'
			peekArray (fromIntegral n) pProps

withMaybeCString :: Maybe String -> (CString -> IO a) -> IO a
withMaybeCString mstr f = case mstr of
	Nothing -> f nullPtr
	Just str -> withCString str f

foreign import ccall "vkEnumerateInstanceExtensionProperties"
	c_vkEnumerateInstanceExtensionProperties ::
	CString -> Ptr #{type uint32_t} -> Ptr ExtensionProperties -> IO Result

destroyInstance ::
	Pointable a => Instance -> Maybe (AllocationCallbacks a) -> IO ()
destroyInstance (Instance pist) mac = case mac of
	Nothing -> c_vkDestroyInstance pist nullPtr
	Just ac -> withAllocationCallbacksPtr ac \pac ->
		c_vkDestroyInstance pist pac

foreign import ccall "vkDestroyInstance" c_vkDestroyInstance ::
	Ptr Instance -> Ptr I.AllocationCallbacks -> IO ()

struct "LayerProperties" #{size VkLayerProperties}
		#{alignment VkLayerProperties} [
	("layerName", ''String,
		[| peekCString . #{ptr VkLayerProperties, layerName} |],
		[| \p s -> pokeCStringLen
			#{const VK_MAX_EXTENSION_NAME_SIZE}
			(#{ptr VkLayerProperties, layerName} p) s |]),
	("specVersion", ''#{type uint32_t},
		[| #{peek VkLayerProperties, specVersion} |],
		[| #{poke VkLayerProperties, specVersion} |]),
	("implementationVersion", ''#{type uint32_t},
		[| #{peek VkLayerProperties, implementationVersion} |],
		[| #{poke VkLayerProperties, implementationVersion} |]),
	("description", ''String,
		[| peekCString . #{ptr VkLayerProperties, description} |],
		[| \p s -> pokeCStringLen
			#{const VK_MAX_DESCRIPTION_SIZE}
			(#{ptr VkLayerProperties, description} p) s |]) ]
	[''Show, ''Storable]

enumerateInstanceLayerProperties :: IO [LayerProperties]

enumerateInstanceLayerProperties = alloca \pn -> do
	r <- c_vkEnumerateInstanceLayerProperties pn nullPtr
	throwUnlessSuccess r
	n <- peek pn
	allocaArray (fromIntegral n) \pProps -> do
		r' <- c_vkEnumerateInstanceLayerProperties pn pProps
		throwUnlessSuccess r'
		peekArray (fromIntegral n) pProps

foreign import ccall "vkEnumerateInstanceLayerProperties"
	c_vkEnumerateInstanceLayerProperties ::
	Ptr #{type uint32_t} -> Ptr LayerProperties -> IO Result

enum "ObjectType" ''#{type VkObjectType} [''Show, ''Storable] [
	("ObjectTypeUnknown", #{const VK_OBJECT_TYPE_UNKNOWN}),
	("ObjectTypeInstance", #{const VK_OBJECT_TYPE_INSTANCE}),
	("ObjectTypePhysicalDevice", #{const VK_OBJECT_TYPE_PHYSICAL_DEVICE}),
	("ObjectTypeDevice", #{const VK_OBJECT_TYPE_DEVICE}),
	("ObjectTypeQueue", #{const VK_OBJECT_TYPE_QUEUE}),
	("ObjectTypeSemaphore", #{const VK_OBJECT_TYPE_SEMAPHORE}),
	("ObjectTypeCommandBuffer", #{const VK_OBJECT_TYPE_COMMAND_BUFFER}),
	("ObjectTypeFence", #{const VK_OBJECT_TYPE_FENCE}),
	("ObjectTypeDeviceMemory", #{const VK_OBJECT_TYPE_DEVICE_MEMORY}),
	("ObjectTypeBuffer", #{const VK_OBJECT_TYPE_BUFFER}),
	("ObjectTypeImage", #{const VK_OBJECT_TYPE_IMAGE}),
	("ObjectTypeEvent", #{const VK_OBJECT_TYPE_EVENT}),
	("ObjectTypeQueryPool", #{const VK_OBJECT_TYPE_QUERY_POOL}),
	("ObjectTypeBufferView", #{const VK_OBJECT_TYPE_BUFFER_VIEW}),
	("ObjectTypeImageView", #{const VK_OBJECT_TYPE_IMAGE_VIEW}),
	("ObjectTypeShaderModule", #{const VK_OBJECT_TYPE_SHADER_MODULE}),
	("ObjectTypePipelineCache", #{const VK_OBJECT_TYPE_PIPELINE_CACHE}),
	("ObjectTypePipelineLayout", #{const VK_OBJECT_TYPE_PIPELINE_LAYOUT}),
	("ObjectTypeRenderPass", #{const VK_OBJECT_TYPE_RENDER_PASS}),
	("ObjectTypePipeline", #{const VK_OBJECT_TYPE_PIPELINE}),
	("ObjectTypeDescriptorSetLayout",
		#{const VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT}),
	("ObjectTypeSampler", #{const VK_OBJECT_TYPE_SAMPLER}),
	("ObjectTypeDescriptorPool", #{const VK_OBJECT_TYPE_DESCRIPTOR_POOL}),
	("ObjectTypeDescriptorSet", #{const VK_OBJECT_TYPE_DESCRIPTOR_SET}),
	("ObjectTypeFramebuffer", #{const VK_OBJECT_TYPE_FRAMEBUFFER}),
	("ObjectTypeCommandPool", #{const VK_OBJECT_TYPE_COMMAND_POOL}),
	("ObjectTypeSamplerYcbcrConversion",
		#{const VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION}),
	("ObjectTypeDescriptorUpdateTemplate",
		#{const VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE}),
	("ObjectTypeSurfaceKhr", #{const VK_OBJECT_TYPE_SURFACE_KHR}),
	("ObjectTypeSwapchainKhr", #{const VK_OBJECT_TYPE_SWAPCHAIN_KHR}),
	("ObjectTypeDisplayKhr", #{const VK_OBJECT_TYPE_DISPLAY_KHR}),
	("ObjectTypeDisplayModeKhr", #{const VK_OBJECT_TYPE_DISPLAY_MODE_KHR}),
	("ObjectTypeDebugReportCallbackExt",
		#{const VK_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT}),
	#ifdef VK_ENABLE_BETA_EXTENSIONS
	("ObjectTypeVideoSessionKhr",
		#{const VK_OBJECT_TYPE_VIDEO_SESSION_KHR}),
	("ObjectTypeVideoSessionParametersKhr",
		#{const VK_OBJECT_TYPE_VIDEO_SESSION_PARAMETERS_KHR}),
	#endif
	("ObjectTypeCuModuleNvx", #{const VK_OBJECT_TYPE_CU_MODULE_NVX}),
	("ObjectTypeDebugUtilsMessengerExt",
		#{const VK_OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT}),
	("ObjectTypeAccelerationStructureKhr",
		#{const VK_OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR}),
	("ObjectTypeValidationCacheExt",
		#{const VK_OBJECT_TYPE_VALIDATION_CACHE_EXT}),
	("ObjectTypeAccelerationStructureNv",
		#{const VK_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV}),
	("ObjectTypePerformanceConfigurationIntel",
		#{const VK_OBJECT_TYPE_PERFORMANCE_CONFIGURATION_INTEL}),
	("ObjectTypeDeferredOperationKhr",
		#{const VK_OBJECT_TYPE_DEFERRED_OPERATION_KHR}),
	("ObjectTypeIndirectCommandsLayoutNv",
		#{const VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NV}),
	("ObjectTypePrivateDataSlotExt",
		#{const VK_OBJECT_TYPE_PRIVATE_DATA_SLOT_EXT}),
--	("ObjectTypeBufferCollectionFuchsia",
--		#{const VK_OBJECT_TYPE_BUFFER_COLLECTION_FUCHSIA})
	("ObjectTypeDescriptorUpdateTemplateKhr",
		#{const VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR}),
	("ObjectTypeSamplerYcbcrConversionKhr",
		#{const VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_KHR}),
	("ObjectTypeMaxEnum", #{const VK_OBJECT_TYPE_MAX_ENUM})
	]
