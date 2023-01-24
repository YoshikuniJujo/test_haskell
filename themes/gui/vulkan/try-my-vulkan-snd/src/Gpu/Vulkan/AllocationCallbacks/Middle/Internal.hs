{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.AllocationCallbacks.Middle.Internal where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable.PeekPoke
import Foreign.Pointable hiding (NullPtr)
import Control.Monad.Cont

import qualified Gpu.Vulkan.AllocationCallbacks.Core as C

data A a = A {
	allocationCallbacksUserData :: a,
	allocationCallbacksFnAllocation :: C.FnAllocationFunction a,
	allocationCallbacksFnReallocation :: C.FnReallocationFunction a,
	allocationCallbacksFnFree :: C.FnFreeFunction a,
	allocationCallbacksFnInternalAllocation ::
		C.FnInternalAllocationNotification a,
	allocationCallbacksFnInternalFree :: C.FnInternalFreeNotification a }

maybeToCore :: Pokable n => Maybe (A n) -> ContT r IO (Ptr C.A)
maybeToCore = \case Nothing -> pure NullPtr; Just ac -> toCore ac

toCore :: Pokable n => A n -> ContT r IO (Ptr C.A)
toCore ac = ContT
	$ \f -> withA ac \(C.A_ fac) ->
		withForeignPtr fac f

withA :: Pokable a =>
	A a -> (C.A -> IO b) -> IO b
withA ac f = withPoked ud \pud -> do
	pal <- C.wrapAllocationFunction al
	pral <- C.wrapReallocationFunction ral
	pfr <- C.wrapFreeFunction fr
	pial <- C.wrapInternalAllocationNotification ial
	pifr <- C.wrapInternalFreeNotification ifr
	f (C.A (castPtr pud) pal pral pfr pial pifr) <* do
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

maybeToCore' :: WithPoked n => Maybe (A n) -> (Ptr C.A -> IO b) -> IO ()
maybeToCore' = \case Nothing -> (() <$) . ($ NullPtr); Just ac -> toCore' ac

toCore' :: WithPoked n => A n -> (Ptr C.A -> IO b) -> IO ()
toCore' ac f = withA' ac \(C.A_ fac) -> withForeignPtr fac $ (() <$) . f

withA' :: WithPoked a =>
	A a -> (C.A -> IO b) -> IO ()
withA' ac f = withPoked' ud \pud -> do
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
