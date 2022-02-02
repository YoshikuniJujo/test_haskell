{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Fence where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Cont

import Vulkan.Base
import Vulkan.Exception
import Vulkan.AllocationCallbacks
import Vulkan.Device
import Vulkan.FenceCreateFlagBits

import qualified Vulkan.AllocationCallbacks.Internal as I
import qualified Vulkan.Fence.Internal as I

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: FenceCreateFlags }
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

data FenceTag
newtype Fence = Fence (Ptr FenceTag) deriving (Show, Storable)

pattern FenceNullHandle :: Fence
pattern FenceNullHandle <- Fence NullHandle where
	FenceNullHandle = Fence NullHandle

create :: (Storable n, Storable n') =>
	Device -> CreateInfo n -> Maybe (AllocationCallbacks n') -> IO Fence
create dvc ci mac = ($ pure) $ runContT do
	I.CreateInfo_ fci <- createInfoToC ci
	pci <- ContT $ withForeignPtr fci
	pac <- case mac of
		Nothing -> pure NullPtr
		Just ac -> ContT $ withAllocationCallbacksPtr ac
	pf <- ContT alloca
	lift do	r <- c_vkCreateFence dvc pci pac pf
		throwUnlessSuccess r
		peek pf

foreign import ccall "vkCreateFence" c_vkCreateFence ::
	Device -> Ptr I.CreateInfo -> Ptr I.AllocationCallbacks ->
	Ptr Fence -> IO Result

destroy :: Storable n =>
	Device -> Fence -> Maybe (AllocationCallbacks n) -> IO ()
destroy dvc fnc mac = ($ pure) $ runContT do
	pac <- case mac of
		Nothing -> pure NullPtr
		Just ac -> ContT $ withAllocationCallbacksPtr ac
	lift $ c_vkDestroyFence dvc fnc pac

foreign import ccall "vkDestroyFence" c_vkDestroyFence ::
	Device -> Fence -> Ptr I.AllocationCallbacks -> IO ()
