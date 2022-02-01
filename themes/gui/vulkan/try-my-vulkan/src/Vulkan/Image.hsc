{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Image (
	module Vulkan.Image,
	module Vulkan.Image.EnableBetaExtensions
	) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Cont

import Vulkan.Base
import Vulkan.Exception
import Vulkan.Format
import Vulkan.AllocationCallbacks
import Vulkan.Device
import Vulkan.Image.EnableBetaExtensions hiding (
	pattern ImageUsageVideoDecodeDstBit,
	pattern ImageUsageVideoDecodeSrcBit,
	pattern ImageUsageVideoDecodeDpbBit,
	pattern ImageUsageVideoEncodeDstBit,
	pattern ImageUsageVideoEncodeSrcBit,
	pattern ImageUsageVideoEncodeDpbBit )

import qualified Vulkan.AllocationCallbacks.Internal as I
import qualified Vulkan.Image.Internal as I

data ImageViewCreateInfo n = ImageViewCreateInfo {
	imageViewCreateInfoNext :: Maybe n,
	imageViewCreateInfoFlags :: I.ImageViewCreateFlags,
	imageViewCreateInfoImage :: I.Image,
	imageViewCreateInfoViewType :: I.ImageViewType,
	imageViewCreateInfoFormat :: Format,
	imageViewCreateInfoComponents :: I.ComponentMapping,
	imageViewCreateInfoSubresourceRange :: I.ImageSubresourceRange }
	deriving Show

imageViewCreateInfoToC :: Pointable n =>
	ImageViewCreateInfo n -> (I.ImageViewCreateInfo -> IO a) -> IO a
imageViewCreateInfoToC ImageViewCreateInfo {
	imageViewCreateInfoNext = mnxt,
	imageViewCreateInfoFlags = flgs,
	imageViewCreateInfoImage = img,
	imageViewCreateInfoViewType = ivt,
	imageViewCreateInfoFormat = fmt,
	imageViewCreateInfoComponents = cmp,
	imageViewCreateInfoSubresourceRange = srr } = runContT do
	(castPtr -> pnxt) <- case mnxt of
		Nothing -> pure NullPtr
		Just nxt -> ContT $ withPointer nxt
	pure $ I.ImageViewCreateInfo () pnxt flgs img ivt fmt cmp srr

newtype ImageView = ImageVew (Ptr ImageView) deriving (Show, Storable)
type PtrImageView = Ptr ImageView

createImageView :: (Pointable n, Pointable n') => Device ->
	ImageViewCreateInfo n -> Maybe (AllocationCallbacks n') -> IO ImageView
createImageView dvc ivci mac = ($ pure) $ runContT do
	I.ImageViewCreateInfo_ fivci <- ContT $ imageViewCreateInfoToC ivci
	pivci <- ContT $ withForeignPtr fivci
	pac <- case mac of
		Nothing -> pure NullPtr
		Just ac -> ContT $ withAllocationCallbacksPtr ac
	piv <- ContT alloca
	lift do	r <- c_vkCreateImageView dvc pivci pac piv
		throwUnlessSuccess r
		peek piv

foreign import ccall "vkCreateImageView" c_vkCreateImageView ::
	Device -> Ptr I.ImageViewCreateInfo -> Ptr I.AllocationCallbacks ->
	Ptr ImageView -> IO Result

destroyImageView :: Pointable n =>
	Device -> ImageView -> Maybe (AllocationCallbacks n) -> IO ()
destroyImageView dvc iv mac = ($ pure) $ runContT do
	pac <- case mac of
		Nothing -> pure NullPtr
		Just ac -> ContT $ withAllocationCallbacksPtr ac
	lift $ c_vkDestroyImageView dvc iv pac

foreign import ccall "vkDestroyImageView" c_vkDestroyImageView ::
	Device -> ImageView -> Ptr I.AllocationCallbacks -> IO ()
