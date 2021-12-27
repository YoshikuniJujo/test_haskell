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
import Foreign.ForeignPtr hiding (newForeignPtr, addForeignPtrFinalizer)
import Foreign.C.String
import Foreign.C.Enum
import Foreign.C.Struct
import Data.Word

import Vulkan.Exception

import qualified Vulkan.Internal as I

#include <vulkan/vulkan.h>

data ApplicationInfo a = ApplicationInfo {
	applicationInfoNext :: Maybe a,
	applicationInfoApplicationName :: String,
	applicationInfoApplicationVersion :: I.ApiVersion,
	applicationInfoEngineName :: String,
	applicationInfoEngineVersion :: I.ApiVersion,
	applicationInfoApiVersion :: I.ApiVersion }
	deriving Show

class Pointable a where
	withPointer :: a -> (Ptr a -> IO b) -> IO b
	fromPointer :: Ptr a -> IO a

instance {-# OVERLAPPABLE #-} Storable a => Pointable a where
	withPointer x f = alloca \p -> poke p x >> f p
	fromPointer = peek

withMaybePointer :: Pointable a => Maybe a -> (Ptr a -> IO b) -> IO b
withMaybePointer mx f = case mx of
	Nothing -> f nullPtr
	Just x -> withPointer x f

withApplicationInfo :: Pointable a =>
	ApplicationInfo a -> (I.ApplicationInfo -> IO b) -> IO b
withApplicationInfo ai f = withMaybePointer (applicationInfoNext ai) \pnxt ->
	withCString (applicationInfoApplicationName ai) \canm ->
		withCString (applicationInfoEngineName ai) \cenm ->
			f I.ApplicationInfo {
				I.applicationInfoSType = (),
				I.applicationInfoPNext = castPtr pnxt,
				I.applicationInfoPApplicationName = canm,
				I.applicationInfoApplicationVersion =
					applicationInfoApplicationVersion ai,
				I.applicationInfoPEngineName = cenm,
				I.applicationInfoEngineVersion =
					applicationInfoEngineVersion ai,
				I.applicationInfoApiVersion =
					applicationInfoApiVersion ai }

data AllocationCallbacks a = AllocationCallbacks {
	allocationCallbacksUserData :: a,
	allocationCallbacksFnAllocation :: I.FnAllocationFunction a,
	allocationCallbacksFnReallocation :: I.FnReallocationFunction a,
	allocationCallbacksFnFree :: I.FnFreeFunction a,
	allocationCallbacksFnInternalAllocation ::
		I.FnInternalAllocationNotification a,
	allocationCallbacksFnInternalFree :: I.FnInternalFreeNotification a }

withAllocationCallbacksPtr :: Pointable a =>
	AllocationCallbacks a -> (Ptr I.AllocationCallbacks -> IO b) -> IO b
withAllocationCallbacksPtr ac f =
	withAllocationCallbacks ac \(I.AllocationCallbacks_ fac) ->
		withForeignPtr fac f

withAllocationCallbacks :: Pointable a =>
	AllocationCallbacks a -> (I.AllocationCallbacks -> IO b) -> IO b
withAllocationCallbacks ac f = withPointer ud \pud -> do
	pal <- I.wrapAllocationFunction al
	pral <- I.wrapReallocationFunction ral
	pfr <- I.wrapFreeFunction fr
	pial <- I.wrapInternalAllocationNotification ial
	pifr <- I.wrapInternalFreeNotification ifr
	f (I.AllocationCallbacks (castPtr pud) pal pral pfr pial pifr) <* do
		freeHaskellFunPtr pal
		freeHaskellFunPtr pral
		freeHaskellFunPtr pfr
		freeHaskellFunPtr pial
		freeHaskellFunPtr pifr
	where
	ud = allocationCallbacksUserData ac
	al = allocationCallbacksFnAllocation ac
	ral = allocationCallbacksFnReallocation ac
	fr = allocationCallbacksFnFree ac
	ial = allocationCallbacksFnInternalAllocation ac
	ifr = allocationCallbacksFnInternalFree ac

data InstanceCreateInfo a b = InstanceCreateInfo {
	instanceCreateInfoNext :: Maybe a,
	instanceCreateInfoFlags :: I.InstanceCreateFlags,
	instanceCreateInfoApplicationInfo :: ApplicationInfo b,
	instanceCreateInfoEnabledLayers :: [String],
	instanceCreateInfoEnabledExtensions :: [String] }
	deriving Show

withInstanceCreateInfoPtr :: (Pointable a, Pointable b) =>
	InstanceCreateInfo a b -> (Ptr I.InstanceCreateInfo -> IO c) -> IO c
withInstanceCreateInfoPtr ici f =
	withInstanceCreateInfo ici \(I.InstanceCreateInfo_ fici) ->
		withForeignPtr fici f

withInstanceCreateInfo :: (Pointable a, Pointable b) =>
	InstanceCreateInfo a b -> (I.InstanceCreateInfo -> IO c) -> IO c
withInstanceCreateInfo ic f = withMaybePointer (instanceCreateInfoNext ic) \pnxt ->
	withApplicationInfo (instanceCreateInfoApplicationInfo ic) \(I.ApplicationInfo_ fai) ->
		I.withCStrings (instanceCreateInfoEnabledLayers ic) \eln els ->
			I.withCStrings (instanceCreateInfoEnabledExtensions ic) \en es ->
				withForeignPtr fai \pai ->
					f $ I.InstanceCreateInfo () (castPtr pnxt)
						(instanceCreateInfoFlags ic) pai
						eln els en es

newtype Instance = Instance (Ptr Instance) deriving (Show, Storable)

createInstance :: (Pointable a, Pointable b, Storable c) =>
	InstanceCreateInfo a b -> Maybe (AllocationCallbacks c) -> IO Instance
createInstance ici mac = alloca \pist -> withInstanceCreateInfoPtr ici \pici -> do
	r <- case mac of
		Nothing -> c_vkCreateInstance pici nullPtr pist
		Just ac -> withAllocationCallbacksPtr ac \pac ->
			c_vkCreateInstance pici pac pist
	throwUnlessSuccess r
	peek pist

foreign import ccall "vkCreateInstance" c_vkCreateInstance ::
	Ptr I.InstanceCreateInfo -> Ptr I.AllocationCallbacks -> Ptr Instance ->
	IO Result

pokeCString :: Int -> CString -> String -> IO ()
pokeCString n cs str = withCString str \cs_ -> copyBytes cs cs_ n

struct "ExtensionProperties" #{size VkExtensionProperties}
		#{alignment VkExtensionProperties} [
	("extensionName", ''String,
		[| peekCString . #{ptr VkExtensionProperties, extensionName} |],
		[| \p s -> pokeCString
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
		[| \p s -> pokeCString
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
		[| \p s -> pokeCString
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
