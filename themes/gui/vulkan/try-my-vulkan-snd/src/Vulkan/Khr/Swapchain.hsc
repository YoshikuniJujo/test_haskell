{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Khr.Swapchain where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Foreign.Pointable
import Control.Monad.Cont
import Data.Word

import qualified Data.Text as T

import Vulkan.Enum
import Vulkan.Base
import Vulkan.Exception
import Vulkan.Exception.Enum
import Vulkan.Khr.Enum
import Vulkan.Khr.Swapchain.Enum
import Vulkan.Khr.Surface.Enum

import qualified Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Vulkan.Device as Device
import qualified Vulkan.Image.Enum as Image
import qualified Vulkan.Image as Image
import qualified Vulkan.Khr.Surface as Surface
import qualified Vulkan.Core as C
import qualified Vulkan.Khr.Swapchain.Core as C
import qualified Vulkan.Format.Enum as Format

#include <vulkan/vulkan.h>

extensionName :: T.Text
extensionName = #{const_str VK_KHR_SWAPCHAIN_EXTENSION_NAME}

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoSurface :: Surface.S,
	createInfoMinImageCount :: Word32,
	createInfoImageFormat :: Format.F,
	createInfoImageColorSpace :: ColorSpace,
	createInfoImageExtent :: C.Extent2d,
	createInfoImageArrayLayers :: Word32,
	createInfoImageUsage :: Image.UsageFlags,
	createInfoImageSharingMode :: SharingMode,
	createInfoQueueFamilyIndices :: [Word32],
	createInfoPreTransform :: TransformFlagBits,
	createInfoCompositeAlpha :: CompositeAlphaFlagBits,
	createInfoPresentMode :: PresentMode,
	createInfoClipped :: Bool,
	createInfoOldSwapchain :: Maybe S }
	deriving Show

createInfoToCore :: Pointable n => CreateInfo n -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoSurface = Surface.S sfc,
	createInfoMinImageCount = mic,
	createInfoImageFormat = Format.F ifmt,
	createInfoImageColorSpace = ColorSpace ics,
	createInfoImageExtent = iex,
	createInfoImageArrayLayers = ials,
	createInfoImageUsage = Image.UsageFlagBits iusg,
	createInfoImageSharingMode = SharingMode ism,
	createInfoQueueFamilyIndices = qfis,
	createInfoPreTransform = TransformFlagBits pt,
	createInfoCompositeAlpha = CompositeAlphaFlagBits caf,
	createInfoPresentMode = PresentMode pm,
	createInfoClipped = clpd,
	createInfoOldSwapchain = mos
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	pqfis <- ContT $ allocaArray qfic
	lift $ pokeArray pqfis qfis
	let	os = case mos of
			Nothing -> wordPtrToPtr $ WordPtr #{const VK_NULL_HANDLE}
			Just (S s) -> s
		C.CreateInfo_ fCreateInfo = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt,
			C.createInfoFlags = flgs,
			C.createInfoSurface = sfc,
			C.createInfoMinImageCount = mic,
			C.createInfoImageFormat = ifmt,
			C.createInfoImageColorSpace = ics,
			C.createInfoImageExtent = iex,
			C.createInfoImageArrayLayers = ials,
			C.createInfoImageUsage = iusg,
			C.createInfoImageSharingMode = ism,
			C.createInfoQueueFamilyIndexCount = fromIntegral qfic,
			C.createInfoPQueueFamilyIndices = pqfis,
			C.createInfoPreTransform = pt,
			C.createInfoCompositeAlpha = caf,
			C.createInfoPresentMode = pm,
			C.createInfoClipped = boolToBool32 clpd,
			C.createInfoOldSwapchain = os }
	ContT $ withForeignPtr fCreateInfo
	where qfic = length qfis

newtype S = S { unS :: C.S } deriving Show

create :: (Pointable n, Pointable n') =>
	Device.D -> CreateInfo n -> Maybe (AllocationCallbacks.A n') -> IO S
create (Device.D dvc) ci mac = ($ pure) . runContT $ S <$> do
	pci <- createInfoToCore ci
	pac <- AllocationCallbacks.maybeToCore mac
	psc <- ContT alloca
	lift do	r <- C.create dvc pci pac psc
		throwUnlessSuccess $ Result r
		peek psc

destroy :: Pointable n =>
	Device.D -> S -> Maybe (AllocationCallbacks.A n) -> IO ()
destroy (Device.D dvc) (S sc) mac = ($ pure) . runContT
	$ lift . C.destroy dvc sc =<< AllocationCallbacks.maybeToCore mac

getImages :: Device.D -> S -> IO [Image.I]
getImages (Device.D dvc) (S sc) = ($ pure) . runContT $ (Image.I <$>) <$> do
	pSwapchainImageCount <- ContT alloca
	(fromIntegral -> swapchainImageCount) <- lift do
		r <- C.getImages dvc sc pSwapchainImageCount NullPtr
		throwUnlessSuccess $ Result r
		peek pSwapchainImageCount
	pSwapchainImages <- ContT $ allocaArray swapchainImageCount
	lift do	r <- C.getImages dvc sc pSwapchainImageCount pSwapchainImages
		throwUnlessSuccess $ Result r
		peekArray swapchainImageCount pSwapchainImages
