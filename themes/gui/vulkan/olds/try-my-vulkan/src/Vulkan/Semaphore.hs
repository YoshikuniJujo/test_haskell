{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Semaphore where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Cont

import Vulkan.Base
import Vulkan.Exception
import Vulkan.AllocationCallbacks
import Vulkan.Device

import qualified Vulkan.AllocationCallbacks.Internal as I
import qualified Vulkan.Semaphore.Internal as I

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: I.CreateFlags }
	deriving Show

createInfoToC :: Pointable n => CreateInfo n -> ContT r IO I.CreateInfo
createInfoToC CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs } = do
	(castPtr -> pnxt) <- ContT $ withMaybePointer mnxt
	pure I.CreateInfo {
		I.createInfoSType = (),
		I.createInfoPNext = pnxt,
		I.createInfoFlags = flgs }

data SemaphoreTag
newtype Semaphore = Semaphore (Ptr SemaphoreTag) deriving (Show, Storable)
type PtrSemaphore = Ptr Semaphore

pattern SemaphoreNullHandle :: Semaphore
pattern SemaphoreNullHandle <- Semaphore NullHandle where
	SemaphoreNullHandle = Semaphore NullHandle

create :: (Pointable n, Pointable n') =>
	Device -> CreateInfo n -> Maybe (AllocationCallbacks n') -> IO Semaphore
create dvc ci mac = ($ pure) $ runContT do
	I.CreateInfo_ fci <- createInfoToC ci
	pci <- ContT $ withForeignPtr fci
	pac <- case mac of
		Nothing -> pure NullPtr
		Just ac -> ContT $ withAllocationCallbacksPtr ac
	ps <- ContT alloca
	lift do	r <- c_vkCreateSemaphore dvc pci pac ps
		throwUnlessSuccess r
		peek ps

foreign import ccall "vkCreateSemaphore" c_vkCreateSemaphore ::
	Device -> Ptr I.CreateInfo -> Ptr I.AllocationCallbacks ->
	Ptr Semaphore -> IO Result

destroy :: Pointable n =>
	Device -> Semaphore -> Maybe (AllocationCallbacks n) -> IO ()
destroy dvc smp mac = ($ pure) $ runContT do
	pac <- case mac of
		Nothing -> pure NullPtr
		Just ac -> ContT $ withAllocationCallbacksPtr ac
	lift $ c_vkDestroySemaphore dvc smp pac

foreign import ccall "vkDestroySemaphore" c_vkDestroySemaphore ::
	Device -> Semaphore -> Ptr I.AllocationCallbacks -> IO ()
