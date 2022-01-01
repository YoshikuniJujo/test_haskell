{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.AllocationCallbacks where

import Foreign.Ptr
import Foreign.ForeignPtr

import Vulkan.Base

import qualified Vulkan.AllocationCallbacks.Internal as I

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
