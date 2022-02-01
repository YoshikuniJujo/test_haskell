{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.CommandPool where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Cont
import Data.Word

import Vulkan.Base
import Vulkan.Exception
import Vulkan.AllocationCallbacks
import Vulkan.Device
import Vulkan.CommandPoolCreateFlagBits

import qualified Vulkan.AllocationCallbacks.Internal as I
import qualified Vulkan.CommandPool.Internal as I

#include <vulkan/vulkan.h>

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CommandPoolCreateFlags,
	createInfoQueueFamilyIndex :: Word32 }
	deriving Show

createInfoToC :: Pointable n => CreateInfo n -> ContT r IO I.CreateInfo
createInfoToC CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoQueueFamilyIndex = word32ToUint32T -> qfi } = do
	(castPtr -> pnxt) <- ContT $ withMaybePointer mnxt
	pure I.CreateInfo {
		I.createInfoSType = (),
		I.createInfoPNext = pnxt,
		I.createInfoFlags = flgs,
		I.createInfoQueueFamilyIndex = qfi }

data CommandPoolTag
newtype CommandPool = CommandPool (Ptr CommandPoolTag) deriving (Show, Storable)

create :: (Pointable n, Pointable n') =>
	Device -> CreateInfo n -> Maybe (AllocationCallbacks n') ->
	IO CommandPool
create dvc ci mac = ($ pure) $ runContT do
	I.CreateInfo_ fci <- createInfoToC ci
	pci <- ContT $ withForeignPtr fci
	pac <- case mac of
		Nothing -> pure NullPtr
		Just ac -> ContT $ withAllocationCallbacksPtr ac
	pcp <- ContT alloca
	lift do	r <- c_vkCreateCommandPool dvc pci pac pcp
		throwUnlessSuccess r
		peek pcp

foreign import ccall "vkCreateCommandPool" c_vkCreateCommandPool ::
	Device -> Ptr I.CreateInfo -> Ptr I.AllocationCallbacks ->
	Ptr CommandPool -> IO Result

destroy :: Pointable n =>
	Device -> CommandPool -> Maybe (AllocationCallbacks n) -> IO ()
destroy dvc cp mac = ($ pure) $ runContT do
	pac <- case mac of
		Nothing -> pure NullPtr
		Just ac -> ContT $ withAllocationCallbacksPtr ac
	lift $ c_vkDestroyCommandPool dvc cp pac

foreign import ccall "vkDestroyCommandPool" c_vkDestroyCommandPool ::
	Device -> CommandPool -> Ptr I.AllocationCallbacks -> IO ()
