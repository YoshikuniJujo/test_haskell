{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.AllocationCallbacks.Middle.Internal (
	A(..), maybeToCore ) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable.PeekPoke

import qualified Gpu.Vulkan.AllocationCallbacks.Core as C

data A a = A {
	allocationCallbacksUserData :: a,
	allocationCallbacksFnAllocation :: C.FnAllocationFunction a,
	allocationCallbacksFnReallocation :: C.FnReallocationFunction a,
	allocationCallbacksFnFree :: C.FnFreeFunction a,
	allocationCallbacksFnInternalAllocation ::
		C.FnInternalAllocationNotification a,
	allocationCallbacksFnInternalFree :: C.FnInternalFreeNotification a }

maybeToCore :: WithPoked n => Maybe (A n) -> (Ptr C.A -> IO b) -> IO ()
maybeToCore = \case Nothing -> (() <$) . ($ NullPtr); Just ac -> toCore ac

toCore :: WithPoked n => A n -> (Ptr C.A -> IO b) -> IO ()
toCore ac f = withA ac \(C.A_ fac) -> withForeignPtr fac $ (() <$) . f

withA :: WithPoked a =>
	A a -> (C.A -> IO b) -> IO ()
withA ac f = withPoked' ud \pud -> do
	pal <- C.wrapAllocationFunction al
	pral <- C.wrapReallocationFunction ral
	pfr <- C.wrapFreeFunction fr
	pial <- C.wrapInternalAllocationNotification ial
	pifr <- C.wrapInternalFreeNotification ifr
	withPtrS pud \pud' -> f (C.A (castPtr pud') pal pral pfr pial pifr)
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
