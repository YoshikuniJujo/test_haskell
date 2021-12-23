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
import Foreign.Concurrent
import Foreign.C.String
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
	instanceCreateInfoExtensions :: [String] }
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
			I.withCStrings (instanceCreateInfoExtensions ic) \en es ->
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

struct "ExtensionProperties" #{size VkExtensionProperties} [
	("extensionName", ''String,
		[| peekCString . #{ptr VkExtensionProperties, extensionName} |],
		[| \p s -> pokeCString
			#{const VK_MAX_EXTENSION_NAME_SIZE}
			(#{ptr VkExtensionProperties, extensionName} p) s |]),
	("specVersion", ''#{type uint32_t}, [| #{peek VkExtensionProperties, specVersion} |],
		[| #{poke VkExtensionProperties, specVersion} |])
	]
	[''Show, ''Read, ''Eq]

instance Storable ExtensionProperties where
	sizeOf _ = #{size VkExtensionProperties}
	alignment _ = #{alignment VkExtensionProperties}
	peek ps = do
		pd <- malloc
		copyBytes pd ps #{size VkExtensionProperties}
		ExtensionProperties_ <$> newForeignPtr pd (free pd)
	poke pd (ExtensionProperties_ fps) = withForeignPtr fps \ps -> copyBytes pd ps #{size VkExtensionProperties}

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
