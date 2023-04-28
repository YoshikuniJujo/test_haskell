{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.AllocationCallbacks.Middle.Internal (
	A(..), maybeToCore ) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable.PeekPoke
import Control.Monad

import Gpu.Vulkan.AllocationCallbacks.Core qualified as C

data A a = A {
	allocationCallbacksUserData :: a,
	allocationCallbacksFnAllocation :: C.FnAllocationFunction a,
	allocationCallbacksFnReallocation :: C.FnReallocationFunction a,
	allocationCallbacksFnFree :: C.FnFreeFunction a,
	allocationCallbacksFnInternalAllocationFree :: Maybe (
		C.FnInternalAllocationNotification a,
		C.FnInternalFreeNotification a ) }

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
	(pial, pifr) <- do
		case allocationCallbacksFnInternalAllocationFree ac of
			Nothing -> pure (nullFunPtr, nullFunPtr)
			Just (ial, ifr) -> do
				wal <- C.wrapInternalAllocationNotification ial
				wfr <- C.wrapInternalFreeNotification ifr
				pure (wal, wfr)
	withPtrS pud \pud' -> f C.A {
		C.aPUserData = castPtr pud',
		C.aPfnAllocation = pal,
		C.aPfnReallocation = pral,
		C.aPfnFree = pfr,
		C.aPfnInternalAllocation = pial,
		C.aPfnInternalFree = pifr }
	freeHaskellFunPtr pal
	freeHaskellFunPtr pral
	freeHaskellFunPtr pfr
	when (pial /= nullFunPtr) $ freeHaskellFunPtr pial
	when (pifr /= nullFunPtr) $ freeHaskellFunPtr pifr
	where
	ud = allocationCallbacksUserData ac
	al = allocationCallbacksFnAllocation ac
	ral = allocationCallbacksFnReallocation ac
	fr = allocationCallbacksFnFree ac
