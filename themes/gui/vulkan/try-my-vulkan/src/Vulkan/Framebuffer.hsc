{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Framebuffer where

import Foreign.Ptr
import Foreign.Marshal.Array
import Control.Monad.Cont
import Data.Word

import Vulkan.Base
import Vulkan.FramebufferCreateFlagBits
import Vulkan.RenderPass (RenderPass)
import Vulkan.Image (ImageView)

import qualified Vulkan.Framebuffer.Internal as I

#include <vulkan/vulkan.h>

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n, createInfoFlags :: FramebufferCreateFlags,
	createInfoRenderPass :: RenderPass,
	createInfoAttachments :: [ImageView],
	createInfoWidth :: Word32, createInfoHeight :: Word32,
	createInfoLayers :: Word32 }
	deriving Show

word32ToUint32T :: Word32 -> #{type uint32_t}
word32ToUint32T = fromIntegral

createInfoToC :: Pointable n => CreateInfo n -> (I.CreateInfo -> IO a) -> IO a
createInfoToC CreateInfo {
	createInfoNext = mnxt, createInfoFlags = flgs,
	createInfoRenderPass = rp, createInfoAttachments = as,
	createInfoWidth = word32ToUint32T -> w,
	createInfoHeight = word32ToUint32T -> h,
	createInfoLayers = word32ToUint32T -> ls } = runContT do
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
