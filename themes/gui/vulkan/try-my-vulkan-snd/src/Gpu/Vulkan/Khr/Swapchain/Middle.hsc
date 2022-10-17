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

import qualified Gpu.Vulkan.TypeEnum as T
import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.QueueFamily.EnumManual as QueueFamily
import qualified Gpu.Vulkan.Device.Middle.Internal as Device
import qualified Gpu.Vulkan.Image.Middle as Image
import qualified Gpu.Vulkan.Image.Enum as Image
import qualified Gpu.Vulkan.Khr.Surface.Type as Surface
import qualified Gpu.Vulkan.Khr.Surface.Middle as Surface.M
import qualified Gpu.Vulkan.Core as C
import qualified Gpu.Vulkan.Khr.Swapchain.Core as C

#include <vulkan/vulkan.h>

extensionName :: T.Text
extensionName = #{const_str VK_KHR_SWAPCHAIN_EXTENSION_NAME}

data CreateInfoNew n ss (fmt :: T.Format) = CreateInfoNew {
	createInfoNextNew :: Maybe n,
	createInfoFlagsNew :: CreateFlags,
	createInfoSurfaceNew :: Surface.S ss,
	createInfoMinImageCountNew :: Word32,
	createInfoImageColorSpaceNew :: ColorSpace,
	createInfoImageExtentNew :: C.Extent2d,
	createInfoImageArrayLayersNew :: Word32,
	createInfoImageUsageNew :: Image.UsageFlags,
	createInfoImageSharingModeNew :: SharingMode,
	createInfoQueueFamilyIndicesNew :: [QueueFamily.Index],
	createInfoPreTransformNew :: TransformFlagBits,
	createInfoCompositeAlphaNew :: CompositeAlphaFlagBits,
	createInfoPresentModeNew :: PresentMode,
	createInfoClippedNew :: Bool,
	createInfoOldSwapchainNew :: Maybe S }
	deriving Show

createInfoFromNew :: forall n ss fmt . T.FormatToValue fmt =>
	CreateInfoNew n ss fmt -> CreateInfo n ss
createInfoFromNew CreateInfoNew {
	createInfoNextNew = mnxt,
	createInfoFlagsNew = flgs,
	createInfoSurfaceNew = sfc,
	createInfoMinImageCountNew = mic,
	createInfoImageColorSpaceNew = cs,
	createInfoImageExtentNew = ext,
	createInfoImageArrayLayersNew = ials,
	createInfoImageUsageNew = usg,
	createInfoImageSharingModeNew = sm,
	createInfoQueueFamilyIndicesNew = qfis,
	createInfoPreTransformNew = ptfm,
	createInfoCompositeAlphaNew = calph,
	createInfoPresentModeNew = pm,
	createInfoClippedNew = clpd,
	createInfoOldSwapchainNew = osc } = CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoSurface = sfc,
	createInfoMinImageCount = mic,
	createInfoImageFormat = T.formatToValue @fmt,
	createInfoImageColorSpace = cs,
	createInfoImageExtent = ext,
	createInfoImageArrayLayers = ials,
	createInfoImageUsage = usg,
	createInfoImageSharingMode = sm,
	createInfoQueueFamilyIndices = qfis,
	createInfoPreTransform = ptfm,
	createInfoCompositeAlpha = calph,
	createInfoPresentMode = pm,
	createInfoClipped = clpd,
	createInfoOldSwapchain = osc }

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

newtype S = S { unS :: IORef (C.Extent2d, C.S) }

instance Show S where show _ = "Gpu.Vulkan.Khr.Swapchain.Middle.S"

sToCore :: S -> IO C.S
sToCore (S s) = snd <$> readIORef s

sToExtent :: S -> IO C.Extent2d
sToExtent (S s) = fst <$> readIORef s

sFromCore :: C.Extent2d -> C.S -> IO S
sFromCore ex s = S <$> newIORef (ex, s)

createNew :: (Pointable n, Pointable n', T.FormatToValue fmt) => Device.D ->
	CreateInfoNew n ss fmt -> Maybe (AllocationCallbacks.A n') -> IO S
createNew dvc ci mac = create dvc (createInfoFromNew ci) mac

create :: (Pointable n, Pointable n') =>
	Device.D -> CreateInfo n ss -> Maybe (AllocationCallbacks.A n') -> IO S
create (Device.D dvc) ci mac = ($ pure) . runContT $ lift . sFromCore ex =<< do
	pci <- createInfoToCore ci
	pac <- AllocationCallbacks.maybeToCore mac
	psc <- ContT alloca
	lift do	r <- C.create dvc pci pac psc
		throwUnlessSuccess $ Result r
		peek psc
	where ex = createInfoImageExtent ci

recreateNew :: (Pointable n, Pointable c, Pointable d, T.FormatToValue fmt) =>
	Device.D -> CreateInfoNew n ss fmt ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	S -> IO ()
recreateNew dvc = recreate dvc . createInfoFromNew

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
		(_, sco) <- readIORef rs
		writeIORef rs . (ex ,) =<< peek psc
		C.destroy dvc sco pacd
	where ex = createInfoImageExtent ci

destroy :: Pointable n =>
	Device.D -> S -> Maybe (AllocationCallbacks.A n) -> IO ()
destroy (Device.D dvc) sc mac = ($ pure) $ runContT do
	pac <- AllocationCallbacks.maybeToCore mac
	sc' <- lift $ sToCore sc
	lift $ C.destroy dvc sc' pac

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

extent2dTo3d :: C.Extent2d -> C.Extent3d
extent2dTo3d C.Extent2d { C.extent2dWidth = w, C.extent2dHeight = h } =
	C.Extent3d {
		C.extent3dWidth = w, C.extent3dHeight = h, C.extent3dDepth = 1 }
