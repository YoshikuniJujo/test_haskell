{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Framebuffer where

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
import Vulkan.FramebufferCreateFlagBits
import Vulkan.Image (ImageView)

import qualified Vulkan.AllocationCallbacks.Internal as I
import qualified Vulkan.Framebuffer.Internal as I

#include <vulkan/vulkan.h>

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n, createInfoFlags :: FramebufferCreateFlags,
	createInfoRenderPass :: RenderPass,
	createInfoAttachments :: [ImageView],
	createInfoWidth :: Word32, createInfoHeight :: Word32,
	createInfoLayers :: Word32 }
	deriving Show

createInfoToC :: Pointable n => CreateInfo n -> ContT r IO I.CreateInfo
createInfoToC CreateInfo {
	createInfoNext = mnxt, createInfoFlags = flgs,
	createInfoRenderPass = rp, createInfoAttachments = as,
	createInfoWidth = word32ToUint32T -> w,
	createInfoHeight = word32ToUint32T -> h,
	createInfoLayers = word32ToUint32T -> ls } = do
	(castPtr -> pnxt) <- ContT $ withMaybePointer mnxt
	let	ac = length as
	pas <- ContT $ allocaArray ac
	lift $ pokeArray pas as
	pure I.CreateInfo {
		I.createInfoSType = (),
		I.createInfoPNext = pnxt,
		I.createInfoFlags = flgs,
		I.createInfoRenderPass = rp,
		I.createInfoAttachmentCount = fromIntegral ac,
		I.createInfoPAttachments = pas,
		I.createInfoWidth = w, I.createInfoHeight = h,
		I.createInfoLayers = ls }

data FramebufferTag
newtype Framebuffer = Framebuffer (Ptr FramebufferTag) deriving (Show, Storable)

create :: (Pointable n, Pointable n') =>
	Device -> CreateInfo n -> Maybe (AllocationCallbacks n') ->
	IO Framebuffer
create dvc ci mac = ($ pure) $ runContT do
	I.CreateInfo_ fci <- createInfoToC ci
	pci <- ContT $ withForeignPtr fci
	pac <- case mac of
		Nothing -> pure NullPtr
		Just ac -> ContT $ withAllocationCallbacksPtr ac
	pfb <- ContT alloca
	lift do	r <- c_vkCreateFramebuffer dvc pci pac pfb
		throwUnlessSuccess r
		peek pfb

foreign import ccall "vkCreateFramebuffer" c_vkCreateFramebuffer ::
	Device -> Ptr I.CreateInfo -> Ptr I.AllocationCallbacks ->
	Ptr Framebuffer -> IO Result

destroy :: Pointable n =>
	Device -> Framebuffer -> Maybe (AllocationCallbacks n) -> IO ()
destroy dvc fb mac = ($ pure) $ runContT do
	pac <- case mac of
		Nothing -> pure NullPtr
		Just ac -> ContT $ withAllocationCallbacksPtr ac
	lift $ c_vkDestroyFramebuffer dvc fb pac

foreign import ccall "vkDestroyFramebuffer" c_vkDestroyFramebuffer ::
	Device -> Framebuffer -> Ptr I.AllocationCallbacks -> IO ()
