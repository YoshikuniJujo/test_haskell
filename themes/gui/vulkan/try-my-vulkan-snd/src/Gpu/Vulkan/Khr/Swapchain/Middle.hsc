{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Swapchain.Middle where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Foreign.Pointable
import Control.Monad.Cont
import Data.IORef
import Data.Word

import qualified Data.Text as T

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Base
import Gpu.Vulkan.Exception
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Khr.Enum
import Gpu.Vulkan.Khr.Swapchain.Enum
import Gpu.Vulkan.Khr.Surface.Enum

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.QueueFamily.EnumManual as QueueFamily
import qualified Gpu.Vulkan.Device.Middle as Device
import qualified Gpu.Vulkan.Image.Middle as Image
import qualified Gpu.Vulkan.Image.Enum as Image
import qualified Gpu.Vulkan.Khr.Surface.Type as Surface
import qualified Gpu.Vulkan.Khr.Surface.Middle as Surface.M
import qualified Gpu.Vulkan.Core as C
import qualified Gpu.Vulkan.Khr.Swapchain.Core as C

#include <vulkan/vulkan.h>

extensionName :: T.Text
extensionName = #{const_str VK_KHR_SWAPCHAIN_EXTENSION_NAME}

data CreateInfo n ss = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoSurface :: Surface.S ss,
	createInfoMinImageCount :: Word32,
	createInfoImageFormat :: Format,
	createInfoImageColorSpace :: ColorSpace,
	createInfoImageExtent :: C.Extent2d,
	createInfoImageArrayLayers :: Word32,
	createInfoImageUsage :: Image.UsageFlags,
	createInfoImageSharingMode :: SharingMode,
	createInfoQueueFamilyIndices :: [QueueFamily.Index],
	createInfoPreTransform :: TransformFlagBits,
	createInfoCompositeAlpha :: CompositeAlphaFlagBits,
	createInfoPresentMode :: PresentMode,
	createInfoClipped :: Bool,
	createInfoOldSwapchain :: Maybe S }
	deriving Show

createInfoToCore :: Pointable n => CreateInfo n ss -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoSurface = Surface.S (Surface.M.S sfc),
	createInfoMinImageCount = mic,
	createInfoImageFormat = Format ifmt,
	createInfoImageColorSpace = ColorSpace ics,
	createInfoImageExtent = iex,
	createInfoImageArrayLayers = ials,
	createInfoImageUsage = Image.UsageFlagBits iusg,
	createInfoImageSharingMode = SharingMode ism,
	createInfoQueueFamilyIndices = ((\(QueueFamily.Index i) -> i) <$>) -> qfis,
	createInfoPreTransform = TransformFlagBits pt,
	createInfoCompositeAlpha = CompositeAlphaFlagBits caf,
	createInfoPresentMode = PresentMode pm,
	createInfoClipped = clpd,
	createInfoOldSwapchain = mos
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	pqfis <- ContT $ allocaArray qfic
	lift $ pokeArray pqfis qfis
	os <- case mos of
		Nothing -> pure $ wordPtrToPtr $ WordPtr #{const VK_NULL_HANDLE}
		Just s -> lift $ sToCore s
	let	C.CreateInfo_ fCreateInfo = C.CreateInfo {
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

newtype S = S { unS :: IORef C.S }

instance Show S where show s = "Gpu.Vulkan.Khr.Swapchain.Middle.S"

sToCore :: S -> IO C.S
sToCore (S s) = readIORef s

sFromCore :: C.S -> IO S
sFromCore s = S <$> newIORef s

create :: (Pointable n, Pointable n') =>
	Device.D -> CreateInfo n ss -> Maybe (AllocationCallbacks.A n') -> IO S
create (Device.D dvc) ci mac = ($ pure) . runContT $ lift . sFromCore =<< do
	pci <- createInfoToCore ci
	pac <- AllocationCallbacks.maybeToCore mac
	psc <- ContT alloca
	lift do	r <- C.create dvc pci pac psc
		throwUnlessSuccess $ Result r
		peek psc

recreate :: (Pointable n, Pointable c, Pointable d) =>
	Device.D -> CreateInfo n ss ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	S -> IO ()
recreate (Device.D dvc) ci macc macd (S rs) = ($ pure) $ runContT do
	pci <- createInfoToCore ci
	pacc <- AllocationCallbacks.maybeToCore macc
	pacd <- AllocationCallbacks.maybeToCore macd
	psc <- ContT alloca
	lift do	r <- C.create dvc pci pacc psc
		throwUnlessSuccess $ Result r
		sco <- readIORef rs
		writeIORef rs =<< peek psc
		C.destroy dvc sco pacd

destroy :: Pointable n =>
	Device.D -> S -> Maybe (AllocationCallbacks.A n) -> IO ()
destroy (Device.D dvc) sc mac = ($ pure) $ runContT do
	pac <- AllocationCallbacks.maybeToCore mac
	sc' <- lift $ sToCore sc
	lift $ C.destroy dvc sc' pac

getImages :: Device.D -> S -> IO [Image.I]
getImages (Device.D dvc) sc = ($ pure) . runContT $ (Image.I <$>) <$> do
	sc' <- lift $ sToCore sc
	pSwapchainImageCount <- ContT alloca
	(fromIntegral -> swapchainImageCount) <- lift do
		r <- C.getImages dvc sc' pSwapchainImageCount NullPtr
		throwUnlessSuccess $ Result r
		peek pSwapchainImageCount
	pSwapchainImages <- ContT $ allocaArray swapchainImageCount
	lift do	r <- C.getImages dvc sc' pSwapchainImageCount pSwapchainImages
		throwUnlessSuccess $ Result r
		mapM newIORef =<< peekArray swapchainImageCount pSwapchainImages
