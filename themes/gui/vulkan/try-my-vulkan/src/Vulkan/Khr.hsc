{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Khr where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Cont
import Data.Word

import Vulkan.Exception
import Vulkan.Device
import Vulkan.Semaphore
import Vulkan.Fence

import qualified Vulkan.Khr.Swapchain.Internal as I

#include <vulkan/vulkan.h>

acquireNextImage ::
	Device -> I.Swapchain -> Word64 -> Semaphore -> Fence -> IO Word32
acquireNextImage dvc sc (word64ToUint64T -> to) smpr fnc = ($ pure) . runContT
	$ uint32TToWord32 <$> do
		pii <- ContT alloca
		lift do	r <- c_vkAcquireNextImageKHR dvc sc to smpr fnc pii
			throwUnlessSuccess r
			peek pii

word64ToUint64T :: Word64 -> #{type uint64_t}
word64ToUint64T = fromIntegral

uint32TToWord32 :: #{type uint32_t} -> Word32
uint32TToWord32 = fromIntegral

foreign import ccall "vkAcquireNextImageKHR" c_vkAcquireNextImageKHR ::
	Device -> I.Swapchain -> #{type uint64_t} -> Semaphore -> Fence ->
	Ptr #{type uint32_t} -> IO Result
