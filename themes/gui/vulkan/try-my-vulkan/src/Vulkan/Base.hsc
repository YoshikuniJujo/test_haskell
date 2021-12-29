{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Base where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Enum
import Data.Word

import qualified Vulkan.Internal as I

#include <vulkan/vulkan.h>

type ListCFloat = [#{type float}]

class Pointable a where
	withPointer :: a -> (Ptr a -> IO b) -> IO b
	fromPointer :: Ptr a -> IO a

instance {-# OVERLAPPABLE #-} Storable a => Pointable a where
	withPointer x f = alloca \p -> poke p x >> f p
	fromPointer = peek

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

pokeCStringLen :: Int -> CString -> String -> IO ()
pokeCStringLen n cs str = withCString str \cs_ -> copyBytes cs cs_ n

pokeCString :: CString -> String -> IO ()
pokeCString cs str = withCStringLen str \(cs_, ln) -> do
	copyBytes cs cs_ ln
	poke (cs `plusPtr` ln :: CString) 0

enum "Bool32" ''#{type VkBool32} [''Show, ''Storable] [
	("VkFalse", #{const VK_FALSE}), ("VkTrue", #{const VK_TRUE}) ]
