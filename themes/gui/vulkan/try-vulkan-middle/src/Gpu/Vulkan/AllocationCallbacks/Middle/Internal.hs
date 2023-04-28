{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.AllocationCallbacks.Middle.Internal (
	A(..),
	FnAllocationFunction, FnReallocationFunction, C.FnFreeFunction,
	FnInternalAllocationNotification, FnInternalFreeNotification,
	Size, Alignment,
	maybeToCore ) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable.PeekPoke
import Control.Monad
import Data.Word

import Gpu.Vulkan.Enum
import Gpu.Vulkan.AllocationCallbacks.Core qualified as C

data A a = A {
	allocationCallbacksUserData :: Ptr a,
	allocationCallbacksFnAllocation :: FnAllocationFunction a,
	allocationCallbacksFnReallocation :: FnReallocationFunction a,
	allocationCallbacksFnFree :: C.FnFreeFunction a,
	allocationCallbacksFnInternalAllocationFree :: Maybe (
		FnInternalAllocationNotification a,
		FnInternalFreeNotification a ) }

type Size = Word64
type Alignment = Word64

type FnAllocationFunction a =
	Ptr a -> Size -> Alignment -> SystemAllocationScope -> IO (Ptr ())

fnAllocationFunctionToCore :: FnAllocationFunction a -> C.FnAllocationFunction a
fnAllocationFunctionToCore f pud sz algn ascp =
	f pud sz algn (SystemAllocationScope ascp)

type FnReallocationFunction a = Ptr a ->
	Ptr () -> Size -> Alignment -> SystemAllocationScope -> IO (Ptr ())

fnReallocationFunctionToCore ::
	FnReallocationFunction a -> C.FnReallocationFunction a
fnReallocationFunctionToCore f pud po sz algn ascp =
	f pud po sz algn (SystemAllocationScope ascp)

type FnInternalAllocationNotification a = Ptr a ->
	Size -> InternalAllocationType -> SystemAllocationScope -> IO ()

fnInternalAllocationNotificationToCore ::
	FnInternalAllocationNotification a ->
	C.FnInternalAllocationNotification a
fnInternalAllocationNotificationToCore f pud sz iatp ascp =
	f pud sz (InternalAllocationType iatp) (SystemAllocationScope ascp)

type FnInternalFreeNotification a = Ptr a ->
	Size -> InternalAllocationType -> SystemAllocationScope -> IO ()

fnInternalFreeNotificationToCore ::
	FnInternalFreeNotification a -> C.FnInternalFreeNotification a
fnInternalFreeNotificationToCore f pud sz iatp ascp =
	f pud sz (InternalAllocationType iatp) (SystemAllocationScope ascp)

maybeToCore :: -- WithPoked n =>
	Maybe (A n) -> (Ptr C.A -> IO b) -> IO ()
maybeToCore = \case Nothing -> (() <$) . ($ NullPtr); Just ac -> toCore ac

toCore :: -- WithPoked n =>
	A n -> (Ptr C.A -> IO b) -> IO ()
toCore ac f = withA ac \(C.A_ fac) -> withForeignPtr fac $ (() <$) . f

withA :: -- WithPoked a =>
	A a -> (C.A -> IO b) -> IO ()
withA ac f = do -- withPoked' ud \pud -> do
	pal <- C.wrapAllocationFunction $ fnAllocationFunctionToCore al
	pral <- C.wrapReallocationFunction $ fnReallocationFunctionToCore ral
	pfr <- C.wrapFreeFunction fr
	(pial, pifr) <- do
		case allocationCallbacksFnInternalAllocationFree ac of
			Nothing -> pure (nullFunPtr, nullFunPtr)
			Just (ial, ifr) -> do
				wal <- C.wrapInternalAllocationNotification
					$ fnInternalAllocationNotificationToCore ial
				wfr <- C.wrapInternalFreeNotification
					$ fnInternalFreeNotificationToCore ifr
				pure (wal, wfr)
	_ <- ($ pud) \pud' -> f C.A {
--	withPtrS pud \pud' -> f C.A {
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
	pud = allocationCallbacksUserData ac
	al = allocationCallbacksFnAllocation ac
	ral = allocationCallbacksFnReallocation ac
	fr = allocationCallbacksFnFree ac
