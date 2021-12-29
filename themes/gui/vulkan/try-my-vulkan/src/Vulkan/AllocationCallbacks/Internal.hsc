{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.AllocationCallbacks.Internal where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Enum
import Foreign.C.Struct
import Data.Word

import Vulkan.Base

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

enum "InternalAllocationType" ''#{type VkInternalAllocationType}
	[''Show, ''Read, ''Eq]
	[	("InternalAllocationTypeExecutable",
			#{const VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE}) ]

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

type FnAllocationFunction a = Ptr a ->
	#{type size_t} -> #{type size_t} -> SystemAllocationScope -> IO (Ptr ())

type PfnAllocationFunction = FunPtr (FnAllocationFunction ())

wrapAllocationFunction :: FnAllocationFunction a -> IO PfnAllocationFunction
wrapAllocationFunction = wrapAllocationFunctionGen . (. castPtr)

foreign import ccall "wrapper" wrapAllocationFunctionGen ::
	FnAllocationFunction () -> IO PfnAllocationFunction

type FnInternalFreeNotification a = Ptr a -> #{type size_t} ->
	InternalAllocationType -> SystemAllocationScope -> IO ()

type PfnInternalFreeNotification = FunPtr (FnInternalFreeNotification ())

wrapInternalFreeNotification ::
	FnInternalFreeNotification a -> IO PfnInternalFreeNotification
wrapInternalFreeNotification = wrapInternalFreeNotificationGen . (. castPtr)

foreign import ccall "wrapper" wrapInternalFreeNotificationGen ::
	FnInternalFreeNotification () -> IO PfnInternalFreeNotification

struct "AllocationCallbacks" #{size VkAllocationCallbacks}
		#{alignment VkAllocationCallbacks} [
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
