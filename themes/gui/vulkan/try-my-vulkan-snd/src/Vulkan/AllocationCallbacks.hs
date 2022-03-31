{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.AllocationCallbacks where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Pointable
import Control.Monad.Cont

import qualified Vulkan.AllocationCallbacks.Core as C

data A a = A {
	allocationCallbacksUserData :: a,
	allocationCallbacksFnAllocation :: C.FnAllocationFunction a,
	allocationCallbacksFnReallocation :: C.FnReallocationFunction a,
	allocationCallbacksFnFree :: C.FnFreeFunction a,
	allocationCallbacksFnInternalAllocation ::
		C.FnInternalAllocationNotification a,
	allocationCallbacksFnInternalFree :: C.FnInternalFreeNotification a }

nil :: Maybe (A ())
nil = Nothing

maybeToCore :: Pointable n =>
	Maybe (A n) -> ContT r IO (Ptr C.A)
maybeToCore = \case Nothing -> pure NullPtr; Just ac -> toCore ac

toCore :: Pointable n =>
	A n -> ContT r IO (Ptr C.A)
toCore ac = ContT
	$ \f -> withA ac \(C.A_ fac) ->
		withForeignPtr fac f

withA :: Pointable a =>
	A a -> (C.A -> IO b) -> IO b
withA ac f = withPointer ud \pud -> do
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
