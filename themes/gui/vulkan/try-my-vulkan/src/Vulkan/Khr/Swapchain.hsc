{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Khr.Swapchain where

import Prelude
import qualified Prelude as P

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Cont
import Data.Maybe
import Data.List
import Data.Word

import Vulkan.Base
import Vulkan.Format
import Vulkan.Image
import Vulkan.Exception
import Vulkan.AllocationCallbacks
import Vulkan.Device
import Vulkan.Khr.Surface

import qualified Vulkan.Base as Vk
import qualified Vulkan.AllocationCallbacks.Internal as I
import qualified Vulkan.Khr.Swapchain.Internal as I

#include <vulkan/vulkan.h>

swapchainExtensionName :: String
swapchainExtensionName = #{const_str VK_KHR_SWAPCHAIN_EXTENSION_NAME}

data SwapchainCreateInfo n = SwapchainCreateInfo {
	swapchainCreateInfoNext :: Maybe n,
	swapchainCreateInfoFlags :: I.SwapchainCreateFlags,
	swapchainCreateInfoSurface :: Surface,
	swapchainCreateInfoMinImageCount :: #{type uint32_t},
	swapchainCreateInfoImageFormat :: Format,
	swapchainCreateInfoImageColorSpace :: ColorSpace,
	swapchainCreateInfoImageExtent :: Extent2d,
	swapchainCreateInfoImageArrayLayers :: #{type uint32_t},
	swapchainCreateInfoImageUsage :: ImageUsageFlags,
	swapchainCreateInfoImageSharingMode :: SharingMode,
	swapchainCreateInfoQueueFamilyIndices :: [#{type uint32_t}],
	swapchainCreateInfoPreTransform :: SurfaceTransformFlagBits,
	swapchainCreateInfoCompositeAlpha :: CompositeAlphaFlagBits,
	swapchainCreateInfoPresentMode :: PresentMode,
	swapchainCreateInfoClipped :: Bool,
	swapchainCreateInfoOldSwapchain :: Maybe I.Swapchain }
	deriving Show

swapchainCreateInfoToC :: Pointable n => SwapchainCreateInfo n ->
	(I.SwapchainCreateInfo -> IO a) -> IO a
swapchainCreateInfoToC SwapchainCreateInfo {
	swapchainCreateInfoNext = mnxt,
	swapchainCreateInfoFlags = sccfs,
	swapchainCreateInfoSurface = sfc,
	swapchainCreateInfoMinImageCount = mic,
	swapchainCreateInfoImageFormat = fmt,
	swapchainCreateInfoImageColorSpace = cs,
	swapchainCreateInfoImageExtent = ie,
	swapchainCreateInfoImageArrayLayers = ials,
	swapchainCreateInfoImageUsage = iu,
	swapchainCreateInfoImageSharingMode = sm,
	swapchainCreateInfoQueueFamilyIndices = qfis,
	swapchainCreateInfoPreTransform = stfbs,
	swapchainCreateInfoCompositeAlpha = cafbs,
	swapchainCreateInfoPresentMode = pm,
	swapchainCreateInfoClipped = clp,
	swapchainCreateInfoOldSwapchain = mosc } = runContT do
	(castPtr -> pnxt) <- maybe (pure NullPtr) (ContT . withPointer) mnxt
	let	qfic = genericLength qfis
	pqfis <- ContT $ withArray qfis
	pure $ I.SwapchainCreateInfo () pnxt
		sccfs sfc mic fmt cs ie ials iu sm qfic pqfis stfbs cafbs pm
		(boolToVkBool32 clp) (fromMaybe I.SwapchainNull mosc)

boolToVkBool32 :: Bool -> Bool32
boolToVkBool32 = \case P.False -> Vk.False; P.True -> Vk.True

createSwapchain :: (Pointable n, Pointable n') =>
	Device -> SwapchainCreateInfo n -> Maybe (AllocationCallbacks n') -> IO I.Swapchain
createSwapchain dvc sci mac = ($ pure) $ runContT do
	I.SwapchainCreateInfo_ fiscci <- ContT $ swapchainCreateInfoToC sci
	piscci <- ContT $ withForeignPtr fiscci
	piac <- maybe (pure NullPtr) (ContT . withAllocationCallbacksPtr) mac
	psc <- ContT alloca
	lift do	r <- c_vkCreateSwapchainKHR dvc piscci piac psc
		throwUnlessSuccess r
		peek psc

foreign import ccall "vkCreateSwapchainKHR" c_vkCreateSwapchainKHR ::
	Device -> Ptr I.SwapchainCreateInfo -> Ptr I.AllocationCallbacks ->
	Ptr I.Swapchain -> IO Result

destroySwapchain :: Storable n =>
	Device -> I.Swapchain -> Maybe (AllocationCallbacks n) -> IO ()
destroySwapchain dv sc mac = ($ pure) $ runContT do
	piac <- maybe (pure NullPtr) (ContT . withAllocationCallbacksPtr) mac
	lift $ c_vkDestroySwapchainKHR dv sc piac

foreign import ccall "vkDestroySwapchainKHR" c_vkDestroySwapchainKHR ::
	Device -> I.Swapchain -> Ptr I.AllocationCallbacks -> IO ()

getSwapchainImages :: Device -> I.Swapchain -> IO [Image]
getSwapchainImages dv sc = ($ pure) $ runContT do
	pn <- ContT alloca
	n <- lift do
		r <- c_vkGetSwapchainImagesKHR dv sc pn NullPtr
		throwUnlessSuccess r
		fromIntegral <$> peek pn
	pis <- ContT $ allocaArray n
	lift do	r <- c_vkGetSwapchainImagesKHR dv sc pn pis
		throwUnlessSuccess r
		peekArray n pis

foreign import ccall "vkGetSwapchainImagesKHR" c_vkGetSwapchainImagesKHR ::
	Device -> I.Swapchain -> Ptr #{type uint32_t} -> Ptr Image -> IO Result
