{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.AllocationCallbacks.Middle.Internal (

	-- * CREATE AND DESTROY

	create, destroy, Functions, FunctionsInfo(..),

	-- ** Function Types

	FnAllocationFunction, FnReallocationFunction, C.FnFreeFunction,
	FnInternalAllocationNotification, FnInternalFreeNotification,

	-- *** size and alignment

	Size, Alignment,

	-- * APPLY

	apply, A,

	-- * INTERNAL USE

	mToCore

	) where

import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Control.Monad
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.Word

import Gpu.Vulkan.Enum
import Gpu.Vulkan.AllocationCallbacks.Core qualified as C

newtype A a = A C.A deriving Show

data Functions a = Functions {
	aPfnAllocation :: C.PfnAllocationFunction,
	aPfnReallocation :: C.PfnReallocationFunction,
	aPfnFree :: C.PfnFreeFunction,
	aPfnInternalAllocation :: C.PfnInternalAllocationNotification,
	aPfnInternalFree :: C.PfnInternalFreeNotification }
	deriving Show

apply :: Functions a -> Ptr a -> A a
apply a p = A C.A {
	C.aPUserData = castPtr p,
	C.aPfnAllocation = aPfnAllocation a,
	C.aPfnReallocation = aPfnReallocation a,
	C.aPfnFree = aPfnFree a,
	C.aPfnInternalAllocation = aPfnInternalAllocation a,
	C.aPfnInternalFree = aPfnInternalFree a }

create :: FunctionsInfo a -> IO (Functions a)
create = mkCallbacksNew

destroy :: Functions a -> IO ()
destroy a = do
	freeHaskellFunPtr allc
	freeHaskellFunPtr rallc
	freeHaskellFunPtr fr
	when (iallc /= nullFunPtr) $ freeHaskellFunPtr iallc
	when (ifr /= nullFunPtr) $ freeHaskellFunPtr ifr
	where
	allc = aPfnAllocation a
	rallc = aPfnReallocation a
	fr = aPfnFree a
	iallc = aPfnInternalAllocation a
	ifr = aPfnInternalFree a

data FunctionsInfo a = FunctionsInfo {
	functionsInfoFnAllocation :: FnAllocationFunction a,
	functionsInfoFnReallocation :: FnReallocationFunction a,
	functionsInfoFnFree :: C.FnFreeFunction a,
	functionsInfoFnInternalAllocationFree :: Maybe (
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

mToCore :: TPMaybe.M A ma -> (Ptr C.A -> IO b) -> IO ()
mToCore = TPMaybe.maybe ((() <$) . ($ NullPtr)) toCoreNew

toCoreNew :: A a -> (Ptr C.A -> IO b) -> IO ()
toCoreNew (A ac) f = () <$ alloca \p -> poke p ac >> f p

mkCallbacksNew :: FunctionsInfo a -> IO (Functions a)
mkCallbacksNew ac = do
	pal <- C.wrapAllocationFunction $ fnAllocationFunctionToCore al
	pral <- C.wrapReallocationFunction $ fnReallocationFunctionToCore ral
	pfr <- C.wrapFreeFunction fr
	(pial, pifr) <- do
		case functionsInfoFnInternalAllocationFree ac of
			Nothing -> pure (nullFunPtr, nullFunPtr)
			Just (ial, ifr) -> do
				wal <- C.wrapInternalAllocationNotification
					$ fnInternalAllocationNotificationToCore ial
				wfr <- C.wrapInternalFreeNotification
					$ fnInternalFreeNotificationToCore ifr
				pure (wal, wfr)
	pure Functions {
		aPfnAllocation = pal,
		aPfnReallocation = pral,
		aPfnFree = pfr,
		aPfnInternalAllocation = pial,
		aPfnInternalFree = pifr }
	where
	al = functionsInfoFnAllocation ac
	ral = functionsInfoFnReallocation ac
	fr = functionsInfoFnFree ac
