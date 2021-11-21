{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Internal where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Enum
import Foreign.C.Struct
import Data.List
import Data.Word

#include <vulkan/vulkan.h>

enum "SystemAllocationScope" ''#{type VkSystemAllocationScope}
	[''Show, ''Read, ''Eq]
	[	("SystemAllocationScopeCommand",
			#{const VK_SYSTEM_ALLOCATION_SCOPE_COMMAND}),
		("SystemAllocationScopeObject",
			#{const VK_SYSTEM_ALLOCATION_SCOPE_OBJECT}),
		("SystemAllocationScopeCache",
			#{const VK_SYSTEM_ALLOCATION_SCOPE_CACHE}),
		("SystemAllocationScopeDevice",
			#{const VK_SYSTEM_ALLOCATION_SCOPE_DEVICE}),
		("SystemAllocationScopeInstance",
			#{const VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE})
		]

enum "InternalAllocationType" ''#{type VkInternalAllocationType}
	[''Show, ''Read, ''Eq]
	[	("InternalAllocationTypeExecutable",
			#{const VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE}) ]

type FnAllocationFunction a = Ptr a ->
	#{type size_t} -> #{type size_t} -> SystemAllocationScope -> IO (Ptr ())

type PfnAllocationFunction = FunPtr (FnAllocationFunction ())

wrapAllocationFunction :: FnAllocationFunction a -> IO PfnAllocationFunction
wrapAllocationFunction = wrapAllocationFunctionGen . (. castPtr)

foreign import ccall "wrapper" wrapAllocationFunctionGen ::
	FnAllocationFunction () -> IO PfnAllocationFunction

type FnReallocationFunction a = Ptr a -> Ptr () -> 
	#{type size_t} -> #{type size_t} -> SystemAllocationScope -> IO (Ptr ())

type PfnReallocationFunction = FunPtr (FnReallocationFunction ())

wrapReallocationFunction :: FnReallocationFunction a -> IO PfnReallocationFunction
wrapReallocationFunction = wrapReallocationFunctionGen . (. castPtr)

foreign import ccall "wrapper" wrapReallocationFunctionGen ::
	FnReallocationFunction () -> IO PfnReallocationFunction

type FnFreeFunction a = Ptr a -> Ptr () -> IO ()

type PfnFreeFunction = FunPtr (FnFreeFunction ())

wrapFreeFunction :: FnFreeFunction a -> IO PfnFreeFunction
wrapFreeFunction = wrapFreeFunctionGen . (. castPtr)

foreign import ccall "wrapper" wrapFreeFunctionGen ::
	FnFreeFunction () -> IO (PfnFreeFunction)

type FnInternalAllocationNotification a = Ptr a -> #{type size_t} ->
	InternalAllocationType -> SystemAllocationScope -> IO ()

type PfnInternalAllocationNotification =
	FunPtr (FnInternalAllocationNotification ())

wrapInternalAllocationNotification ::
	FnInternalAllocationNotification a ->
	IO PfnInternalAllocationNotification
wrapInternalAllocationNotification =
	wrapInternalAllocationNotificationGen . (. castPtr)

foreign import ccall "wrapper" wrapInternalAllocationNotificationGen ::
	FnInternalAllocationNotification () ->
	IO PfnInternalAllocationNotification

type FnInternalFreeNotification a = Ptr a -> #{type size_t} ->
	InternalAllocationType -> SystemAllocationScope -> IO ()

type PfnInternalFreeNotification = FunPtr (FnInternalFreeNotification ())

wrapInternalFreeNotification ::
	FnInternalFreeNotification a -> IO PfnInternalFreeNotification
wrapInternalFreeNotification = wrapInternalFreeNotificationGen . (. castPtr)

foreign import ccall "wrapper" wrapInternalFreeNotificationGen ::
	FnInternalFreeNotification () -> IO PfnInternalFreeNotification

type PtrVoid = Ptr ()

struct "AllocationCallbacks" #{size VkAllocationCallbacks} [
	("pUserData", ''PtrVoid,
		[| #{peek VkAllocationCallbacks, pUserData} |],
		[| #{poke VkAllocationCallbacks, pUserData} |]),
	("pfnAllocation", ''PfnAllocationFunction,
		[| #{peek VkAllocationCallbacks, pfnAllocation} |],
		[| #{poke VkAllocationCallbacks, pfnAllocation} |]),
	("pfnReallocation", ''PfnReallocationFunction,
		[| #{peek VkAllocationCallbacks, pfnReallocation} |],
		[| #{poke VkAllocationCallbacks, pfnReallocation} |]),
	("pfnFree", ''PfnFreeFunction,
		[| #{peek VkAllocationCallbacks, pfnFree} |],
		[| #{poke VkAllocationCallbacks, pfnFree} |]),
	("pfnInternalAllocation", ''PfnInternalAllocationNotification,
		[| #{peek VkAllocationCallbacks, pfnInternalAllocation} |],
		[| #{poke VkAllocationCallbacks, pfnInternalAllocation} |]),
	("pfnInternalFree", ''PfnInternalFreeNotification,
		[| #{peek VkAllocationCallbacks, pfnInternalFree} |],
		[| #{poke VkAllocationCallbacks, pfnInternalFree} |]) ]
	[''Show]

newtype ApiVersion = ApiVersion #{type uint32_t}
	deriving (Show, Eq, Ord, Storable)

struct "ApplicationInfo" #{size VkApplicationInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkApplicationInfo, sType} p
			(#{const VK_STRUCTURE_TYPE_APPLICATION_INFO} ::
				#{type VkStructureType}) |]),
	("pNext", ''PtrVoid, [| #{peek VkApplicationInfo, pNext} |],
		[| #{poke VkApplicationInfo, pNext} |]),
	("pApplicationName", ''CString,
		[| #{peek VkApplicationInfo, pApplicationName} |],
		[| #{poke VkApplicationInfo, pApplicationName} |]),
	("applicationVersion", ''ApiVersion,
		[| #{peek VkApplicationInfo, applicationVersion} |],
		[| #{poke VkApplicationInfo, applicationVersion} |]),
	("pEngineName", ''CString,
		[| #{peek VkApplicationInfo, pEngineName} |],
		[| #{poke VkApplicationInfo, pEngineName} |]),
	("engineVersion", ''ApiVersion,
		[| #{peek VkApplicationInfo, engineVersion} |],
		[| #{poke VkApplicationInfo, engineVersion} |]),
	("apiVersion", ''ApiVersion,
		[| #{peek VkApplicationInfo, apiVersion} |],
		[| #{poke VkApplicationInfo, apiVersion} |])
	]
	[''Show]

foreign import capi "vulkan/vulkan.h VK_MAKE_API_VERSION" makeApiVersion ::
	Word8 -> Word8 -> Word16 -> Word16 -> ApiVersion

apiVersion1_0 :: ApiVersion
apiVersion1_0 = makeApiVersion 0 1 0 0

enum "InstanceCreateFlags" ''#{type VkInstanceCreateFlags}
	[''Show, ''Read, ''Eq, ''Storable] [("InstanceCreateFlagsZero", 0)]

type PtrApplicationInfo = Ptr ApplicationInfo
type PtrCString = Ptr CString

struct "InstanceCreateInfo" #{size VkInstanceCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkInstanceCreateInfo, sType} p
			(#{const VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO} ::
				#{type VkStructureType}) |]),
	("pNext", ''PtrVoid, [| #{peek VkInstanceCreateInfo, pNext} |],
		[| #{poke VkInstanceCreateInfo, pNext} |]),
	("flags", ''InstanceCreateFlags,
		[| #{peek VkInstanceCreateInfo, flags} |],
		[| #{poke VkInstanceCreateInfo, flags} |]),
	("pApplicationInfo", ''PtrApplicationInfo,
		[| #{peek VkInstanceCreateInfo, pApplicationInfo} |],
		[| #{poke VkInstanceCreateInfo, pApplicationInfo} |]),
	("enabledLayerCount", ''#{type uint32_t},
		[| #{peek VkInstanceCreateInfo, enabledLayerCount} |],
		[| #{poke VkInstanceCreateInfo, enabledLayerCount} |]),
	("ppEnabledLayerNames", ''PtrCString,
		[| #{peek VkInstanceCreateInfo, ppEnabledLayerNames} |],
		[| #{poke VkInstanceCreateInfo, ppEnabledLayerNames} |]),
	("enabledExtensionCount", ''#{type uint32_t},
		[| #{peek VkInstanceCreateInfo, enabledExtensionCount} |],
		[| #{poke VkInstanceCreateInfo, enabledExtensionCount} |]),
	("ppEnabledExtensionNames", ''PtrCString,
		[| #{peek VkInstanceCreateInfo, ppEnabledExtensionNames} |],
		[| #{poke VkInstanceCreateInfo, ppEnabledExtensionNames} |]) ]
	[''Show]

withCStrings :: Num n => [String] -> (n -> Ptr CString -> IO a) -> IO a
withCStrings strs f = do
	cstrs <- newCString `mapM` strs
	withArray cstrs (f $ genericLength strs) <* free `mapM_` cstrs
