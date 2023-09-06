{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Instance where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Struct
import Data.Word

import Vulkan.Base
import Vulkan.Exception
import Vulkan.AllocationCallbacks

import qualified Vulkan.Internal as I
import qualified Vulkan.AllocationCallbacks.Internal as I

#include <vulkan/vulkan.h>

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

data ApplicationInfo a = ApplicationInfo {
	applicationInfoNext :: Maybe a,
	applicationInfoApplicationName :: String,
	applicationInfoApplicationVersion :: I.ApiVersion,
	applicationInfoEngineName :: String,
	applicationInfoEngineVersion :: I.ApiVersion,
	applicationInfoApiVersion :: I.ApiVersion }
	deriving Show

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
