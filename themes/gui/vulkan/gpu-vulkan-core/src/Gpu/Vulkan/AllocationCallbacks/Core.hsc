{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.AllocationCallbacks.Core (
	-- * TYPE
	A, pattern A,
	aPUserData, aPfnAllocation, aPfnReallocation, aPfnFree,
	aPfnInternalAllocation, aPfnInternalFree,

	-- * FUNCTIONS
	-- ** ALLOCATION
	wrapAllocationFunction,
	FnAllocationFunction, PfnAllocationFunction,

	-- ** REALLOCATION
	wrapReallocationFunction,
	FnReallocationFunction, PfnReallocationFunction,
	
	-- ** FREE
	wrapFreeFunction,
	FnFreeFunction, PfnFreeFunction,
	
	-- * INTERNAL NOTIFICATION
	-- ** ALLOCATION
	wrapInternalAllocationNotification,
	FnInternalAllocationNotification, PfnInternalAllocationNotification,
	
	-- ** FREE
	wrapInternalFreeNotification,
	FnInternalFreeNotification, PfnInternalFreeNotification

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word

#include <vulkan/vulkan.h>

---------------------------------------------------------------------------
-- ALLOCATION FUNCTION
-- REALLOCATION FUNCTION
-- FREE FUNCTION
-- INTERNAL ALLOCATION NOTIFICATION
-- INTERNAL FREE NOTIFICATION
---------------------------------------------------------------------------

-- * ALLOCATION FUNCTION

type FnAllocationFunction a = Ptr a -> #{type size_t} ->
	#{type size_t} -> #{type VkSystemAllocationScope} -> IO (Ptr ())

type PfnAllocationFunction = FunPtr (FnAllocationFunction ())

wrapAllocationFunction :: FnAllocationFunction a -> IO PfnAllocationFunction
wrapAllocationFunction = wrapAllocationFunctionGen . (. castPtr)

foreign import ccall "wrapper" wrapAllocationFunctionGen ::
	FnAllocationFunction () -> IO PfnAllocationFunction

-- * REALLOCATION FUNCTION

type FnReallocationFunction a = Ptr a -> Ptr () -> #{type size_t} ->
	#{type size_t} -> #{type VkSystemAllocationScope} -> IO (Ptr ())

type PfnReallocationFunction = FunPtr (FnReallocationFunction ())

wrapReallocationFunction ::
	FnReallocationFunction a -> IO PfnReallocationFunction
wrapReallocationFunction = wrapReallocationFunctionGen . (. castPtr)

foreign import ccall "wrapper" wrapReallocationFunctionGen ::
	FnReallocationFunction () -> IO PfnReallocationFunction

-- * FREE FUNCTION

type FnFreeFunction a = Ptr a -> Ptr () -> IO ()

type PfnFreeFunction = FunPtr (FnFreeFunction ())

wrapFreeFunction :: FnFreeFunction a -> IO PfnFreeFunction
wrapFreeFunction = wrapFreeFunctionGen . (. castPtr)

foreign import ccall "wrapper" wrapFreeFunctionGen ::
	FnFreeFunction () -> IO (PfnFreeFunction)

-- * INTERNAL ALLOCATION NOTIFICATION

type FnInternalAllocationNotification a = Ptr a -> #{type size_t} ->
	#{type VkInternalAllocationType} -> #{type VkSystemAllocationScope} ->
	IO ()

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

-- * INTERNAL FREE NOTIFICATION

type FnInternalFreeNotification a = Ptr a -> #{type size_t} ->
	#{type VkInternalAllocationType} -> #{type VkSystemAllocationScope} ->
	IO ()

type PfnInternalFreeNotification = FunPtr (FnInternalFreeNotification ())

wrapInternalFreeNotification ::
	FnInternalFreeNotification a -> IO PfnInternalFreeNotification
wrapInternalFreeNotification = wrapInternalFreeNotificationGen . (. castPtr)

foreign import ccall "wrapper" wrapInternalFreeNotificationGen ::
	FnInternalFreeNotification () -> IO PfnInternalFreeNotification

-- * ALLOCATION CALLBACKS

struct "A" #{size VkAllocationCallbacks}
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
	[''Show, ''Storable]
