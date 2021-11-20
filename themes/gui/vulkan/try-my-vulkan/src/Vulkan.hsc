{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.ForeignPtr hiding (newForeignPtr, addForeignPtrFinalizer)
import Foreign.Concurrent
import Foreign.C.String
import Foreign.C.Struct
import Control.Exception
import Data.Word

import Vulkan.Exception

import qualified Vulkan.Internal as I

#include <vulkan/vulkan.h>

data ApplicationInfo a = ApplicationInfo {
	applicationInfoNext :: a,
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

withApplicationInfo :: Pointable a =>
	ApplicationInfo a -> (I.ApplicationInfo -> IO b) -> IO b
withApplicationInfo ai f = withPointer (applicationInfoNext ai) \pnxt ->
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
	instanceCreateInfoNext :: a,
	instanceCreateInfoFlags :: I.InstanceCreateFlags,
	instanceCreateInfoApplicationInfo :: ApplicationInfo b,
	instanceCreateInfoEnabledLayers :: [String],
	instanceCreateInfoExtensions :: [String] }
	deriving Show

withInstanceCreateInfo :: (Pointable a, Pointable b) =>
	InstanceCreateInfo a b -> (I.InstanceCreateInfo -> IO c) -> IO c
withInstanceCreateInfo ic f = withPointer (instanceCreateInfoNext ic) \pnxt ->
	withApplicationInfo (instanceCreateInfoApplicationInfo ic) \(I.ApplicationInfo_ fai) ->
		I.withCStrings (instanceCreateInfoEnabledLayers ic) \eln els ->
			I.withCStrings (instanceCreateInfoExtensions ic) \en es ->
				withForeignPtr fai \pai ->
					f $ I.InstanceCreateInfo () (castPtr pnxt)
						(instanceCreateInfoFlags ic) pai
						eln els en es

newtype Instance = Instance (Ptr Instance) deriving Show

{-
foreign import ccall "vkCreateInstance" c_vkCreateInstance ::
	Ptr I.InstanceCreateInfo -> Ptr I.AllocationCallbacks -> Ptr Instance ->
	IO Result
	-}

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
		case r of Success -> pure (); _ -> throw r
		n <- peek pn
		allocaArray (fromIntegral n) \pProps -> do
			r' <- c_vkEnumerateInstanceExtensionProperties cs pn pProps
			case r' of Success -> pure (); _ -> throw r'
			peekArray (fromIntegral n) pProps

withMaybeCString :: Maybe String -> (CString -> IO a) -> IO a
withMaybeCString mstr f = case mstr of
	Nothing -> f nullPtr
	Just str -> withCString str f

foreign import ccall "vkEnumerateInstanceExtensionProperties"
	c_vkEnumerateInstanceExtensionProperties ::
	CString -> Ptr #{type uint32_t} -> Ptr ExtensionProperties -> IO Result
