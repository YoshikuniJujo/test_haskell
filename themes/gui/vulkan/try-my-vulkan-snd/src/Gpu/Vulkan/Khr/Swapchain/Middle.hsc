{-# LANGUAGE BlockArguments, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
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
import qualified Gpu.Vulkan.Device.Middle.Internal as Device
import qualified Gpu.Vulkan.Image.Middle.Internal as Image
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

create :: (Pointable n, Pointable n') =>
	Device.D -> CreateInfo n ss -> Maybe (AllocationCallbacks.A n') -> IO S
create dvc ci mac = create' dvc (createInfoToOld ci) mac

recreate :: (Pointable n, Pointable c, Pointable d) =>
	Device.D -> CreateInfo n ss ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	S -> IO ()
recreate dvc ci macc macd s = recreate' dvc (createInfoToOld ci) macc macd s

newtype S = S { unS :: IORef (C.Extent2d, C.S) }

instance Show S where show _ = "Gpu.Vulkan.Khr.Swapchain.Middle.S"

sToCore :: S -> IO C.S
sToCore (S s) = snd <$> readIORef s

sFromCore :: C.Extent2d -> C.S -> IO S
sFromCore ex s = S <$> newIORef (ex, s)

data CreateInfo' n = CreateInfo' {
	createInfoNext' :: Maybe n,
	createInfoFlags' :: CreateFlags,
	createInfoSurface' :: Surface.M.S,
	createInfoMinImageCount' :: Word32,
	createInfoImageFormat' :: Format,
	createInfoImageColorSpace' :: ColorSpace,
	createInfoImageExtent' :: C.Extent2d,
	createInfoImageArrayLayers' :: Word32,
	createInfoImageUsage' :: Image.UsageFlags,
	createInfoImageSharingMode' :: SharingMode,
	createInfoQueueFamilyIndices' :: [QueueFamily.Index],
	createInfoPreTransform' :: TransformFlagBits,
	createInfoCompositeAlpha' :: CompositeAlphaFlagBits,
	createInfoPresentMode' :: PresentMode,
	createInfoClipped' :: Bool,
	createInfoOldSwapchain' :: Maybe S }
	deriving Show

createInfoToOld :: CreateInfo n ss -> CreateInfo' n
createInfoToOld CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoSurface = Surface.S sfc,
	createInfoMinImageCount = mic,
	createInfoImageFormat = ifmt,
	createInfoImageColorSpace = ics,
	createInfoImageExtent = iext,
	createInfoImageArrayLayers = ials,
	createInfoImageUsage = iusg,
	createInfoImageSharingMode = ism,
	createInfoQueueFamilyIndices = qfis,
	createInfoPreTransform = ptfm,
	createInfoCompositeAlpha = calp,
	createInfoPresentMode = pm,
	createInfoClipped = clpd,
	createInfoOldSwapchain = osc
	} = CreateInfo' {
	createInfoNext' = mnxt,
	createInfoFlags' = flgs,
	createInfoSurface' = sfc,
	createInfoMinImageCount' = mic,
	createInfoImageFormat' = ifmt,
	createInfoImageColorSpace' = ics,
	createInfoImageExtent' = iext,
	createInfoImageArrayLayers' = ials,
	createInfoImageUsage' = iusg,
	createInfoImageSharingMode' = ism,
	createInfoQueueFamilyIndices' = qfis,
	createInfoPreTransform' = ptfm,
	createInfoCompositeAlpha' = calp,
	createInfoPresentMode' = pm,
	createInfoClipped' = clpd,
	createInfoOldSwapchain' = osc }

create' :: (Pointable n, Pointable n') =>
	Device.D -> CreateInfo' n -> Maybe (AllocationCallbacks.A n') -> IO S
create' (Device.D dvc) ci mac = ($ pure) . runContT $ lift . sFromCore ex =<< do
	pci <- createInfoToCoreOld ci
	pac <- AllocationCallbacks.maybeToCore mac
	psc <- ContT alloca
	lift do	r <- C.create dvc pci pac psc
		throwUnlessSuccess $ Result r
		peek psc
	where ex = createInfoImageExtent' ci

recreate' :: (Pointable n, Pointable c, Pointable d) =>
	Device.D -> CreateInfo' n ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	S -> IO ()
recreate' (Device.D dvc) ci macc macd (S rs) = ($ pure) $ runContT do
	pci <- createInfoToCoreOld ci
	pacc <- AllocationCallbacks.maybeToCore macc
	pacd <- AllocationCallbacks.maybeToCore macd
	psc <- ContT alloca
	lift do	r <- C.create dvc pci pacc psc
		throwUnlessSuccess $ Result r
		(_, sco) <- readIORef rs
		writeIORef rs . (ex ,) =<< peek psc
		C.destroy dvc sco pacd
	where ex = createInfoImageExtent' ci

destroy :: Pointable n =>
	Device.D -> S -> Maybe (AllocationCallbacks.A n) -> IO ()
destroy (Device.D dvc) sc mac = ($ pure) $ runContT do
	pac <- AllocationCallbacks.maybeToCore mac
	sc' <- lift $ sToCore sc
	lift $ C.destroy dvc sc' pac

createInfoToCoreOld :: Pointable n => CreateInfo' n -> ContT r IO (Ptr C.CreateInfo)
createInfoToCoreOld CreateInfo' {
	createInfoNext' = mnxt,
	createInfoFlags' = CreateFlagBits flgs,
	createInfoSurface' = Surface.M.S sfc,
	createInfoMinImageCount' = mic,
	createInfoImageFormat' = Format ifmt,
	createInfoImageColorSpace' = ColorSpace ics,
	createInfoImageExtent' = iex,
	createInfoImageArrayLayers' = ials,
	createInfoImageUsage' = Image.UsageFlagBits iusg,
	createInfoImageSharingMode' = SharingMode ism,
	createInfoQueueFamilyIndices' = ((\(QueueFamily.Index i) -> i) <$>) -> qfis,
	createInfoPreTransform' = TransformFlagBits pt,
	createInfoCompositeAlpha' = CompositeAlphaFlagBits caf,
	createInfoPresentMode' = PresentMode pm,
	createInfoClipped' = clpd,
	createInfoOldSwapchain' = mos
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

getImages :: Device.D -> S -> IO [Image.I]
getImages (Device.D dvc) sc = ($ pure) . runContT $ (Image.I <$>) <$> do
	sc' <- lift $ sToCore sc
	ex <- lift $ sToExtent sc
	pSwapchainImageCount <- ContT alloca
	(fromIntegral -> swapchainImageCount) <- lift do
		r <- C.getImages dvc sc' pSwapchainImageCount NullPtr
		throwUnlessSuccess $ Result r
		peek pSwapchainImageCount
	pSwapchainImages <- ContT $ allocaArray swapchainImageCount
	lift do	r <- C.getImages dvc sc' pSwapchainImageCount pSwapchainImages
		throwUnlessSuccess $ Result r
		mapM (newIORef . (extent2dTo3d ex ,))
			=<< peekArray swapchainImageCount pSwapchainImages

sToExtent :: S -> IO C.Extent2d
sToExtent (S s) = fst <$> readIORef s

extent2dTo3d :: C.Extent2d -> C.Extent3d
extent2dTo3d C.Extent2d { C.extent2dWidth = w, C.extent2dHeight = h } =
	C.Extent3d {
		C.extent3dWidth = w, C.extent3dHeight = h, C.extent3dDepth = 1 }
