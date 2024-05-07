{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Swapchain.Middle.Internal (

	-- * EXTENSION NAME

	extensionName,

	-- * CREAET AND DESTROY

	create, recreate, destroy, S, CreateInfo(..),

	-- * GET IMAGES

	getImages,

	-- * INTERNAL USE

	sToCore,

	-- * ACQUIRE NEXT IMAGE

	acquireNextImage, acquireNextImageResult,	-- VK_KHR_swapchain

	-- * QUEUE PRESENT

	queuePresent, PresentInfo(..)			-- VK_KHR_swapchain

	) where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.Word
import Data.IORef

import qualified Data.Text as T

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Base.Middle.Internal
import Gpu.Vulkan.Exception.Middle
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Khr.Surface.Enum
import Gpu.Vulkan.Khr.Swapchain.Enum

import Gpu.Vulkan.AllocationCallbacks.Middle.Internal
	qualified as AllocationCallbacks
import qualified Gpu.Vulkan.QueueFamily.Middle as QueueFamily
import qualified Gpu.Vulkan.Device.Middle.Internal as Device
import qualified Gpu.Vulkan.Image.Middle.Internal as Image
import qualified Gpu.Vulkan.Image.Enum as Image
import qualified Gpu.Vulkan.Khr.Surface.Middle.Internal as Surface.M
import qualified Gpu.Vulkan.Core as C
import qualified Gpu.Vulkan.Khr.Swapchain.Core as C

import qualified Gpu.Vulkan.Device.Middle.Internal as Device.M
import qualified Gpu.Vulkan.Fence.Middle.Internal as Fence
import qualified Gpu.Vulkan.Semaphore.Middle.Internal as Semaphore.M
import Gpu.Vulkan.Queue.Middle.Internal as Queue
import Control.Arrow

#include <vulkan/vulkan.h>

extensionName :: T.Text
extensionName = #{const_str VK_KHR_SWAPCHAIN_EXTENSION_NAME}

newtype S = S { _unS :: IORef (C.Extent2d, C.S) }

instance Show S where show _ = "Gpu.Vulkan.Khr.Swapchain.Middle.S"

sToCore :: S -> IO C.S
sToCore (S s) = snd <$> readIORef s

sFromCore :: C.Extent2d -> C.S -> IO S
sFromCore ex s = S <$> newIORef (ex, s)

data CreateInfo mn = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoSurface :: Surface.M.S,
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

deriving instance Show (TMaybe.M mn) => Show (CreateInfo mn)

create :: WithPoked (TMaybe.M mn) =>
	Device.D -> CreateInfo mn -> TPMaybe.M AllocationCallbacks.A mc -> IO S
create (Device.D dvc) ci mac = sFromCore ex =<< alloca \psc -> do
		createInfoToCoreOld ci \pci ->
			AllocationCallbacks.mToCore mac \pac -> do
				r <- C.create dvc pci pac psc
				throwUnlessSuccess $ Result r
		peek psc
	where ex = createInfoImageExtent ci

recreate :: WithPoked (TMaybe.M mn) =>
	Device.D -> CreateInfo mn ->
	TPMaybe.M AllocationCallbacks.A mc ->
	S -> IO ()
recreate (Device.D dvc) ci macc (S rs) = alloca \psc ->
		createInfoToCoreOld ci \pci ->
		AllocationCallbacks.mToCore macc \pacc -> do
			r <- C.create dvc pci pacc psc
			throwUnlessSuccess $ Result r
			(_, sco) <- readIORef rs
			writeIORef rs . (ex ,) =<< peek psc
			C.destroy dvc sco pacc
	where ex = createInfoImageExtent ci

destroy :: Device.D -> S -> TPMaybe.M AllocationCallbacks.A md -> IO ()
destroy (Device.D dvc) sc mac = AllocationCallbacks.mToCore mac \pac -> do
	sc' <- sToCore sc
	C.destroy dvc sc' pac

createInfoToCoreOld :: WithPoked (TMaybe.M mn) => CreateInfo mn -> (Ptr C.CreateInfo -> IO a) -> IO ()
createInfoToCoreOld CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoSurface = Surface.M.S sfc,
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
	createInfoOldSwapchain = mos } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	allocaArray qfic \pqfis ->
	pokeArray pqfis qfis >>
	let	ci os = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt',
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
			C.createInfoOldSwapchain = os } in
	case mos of
		Nothing -> withPoked (ci . wordPtrToPtr $ WordPtr #{const VK_NULL_HANDLE}) f
		Just s -> sToCore s >>= \os -> withPoked (ci os) f
	where qfic = length qfis

getImages :: Device.D -> S -> IO [Image.I]
getImages (Device.D dvc) sc = ((Image.I <$>) <$>) $ sToCore sc >>= \sc' ->
	sToExtent sc >>= \ex ->
	alloca \pSwapchainImageCount ->
	C.getImages dvc sc' pSwapchainImageCount NullPtr >>= \r ->
	throwUnlessSuccess (Result r) >>
	peek pSwapchainImageCount >>= \(fromIntegral -> swapchainImageCount) ->
	allocaArray swapchainImageCount \pSwapchainImages -> do
		r' <- C.getImages dvc sc' pSwapchainImageCount pSwapchainImages
		throwUnlessSuccess $ Result r'
		mapM (newIORef . (extent2dTo3d ex ,))
			=<< peekArray swapchainImageCount pSwapchainImages

sToExtent :: S -> IO C.Extent2d
sToExtent (S s) = fst <$> readIORef s

extent2dTo3d :: C.Extent2d -> C.Extent3d
extent2dTo3d C.Extent2d { C.extent2dWidth = w, C.extent2dHeight = h } =
	C.Extent3d {
		C.extent3dWidth = w, C.extent3dHeight = h, C.extent3dDepth = 1 }

acquireNextImage :: Device.M.D ->
	S -> Word64 -> Maybe Semaphore.M.S -> Maybe Fence.F -> IO Word32
acquireNextImage = acquireNextImageResult [Success]

acquireNextImageResult :: [Result] -> Device.M.D ->
	S -> Word64 -> Maybe Semaphore.M.S -> Maybe Fence.F -> IO Word32
acquireNextImageResult sccs
	(Device.M.D dvc) sc to msmp mfnc = alloca \pii ->
	sToCore sc >>= \sc' -> do
		r <- C.acquireNextImage dvc sc' to smp fnc pii
		throwUnless sccs $ Result r
		peek pii
	where
	smp = maybe NullHandle (\(Semaphore.M.S s) -> s) msmp
	fnc = maybe NullHandle (\(Fence.F f) -> f) mfnc

---------------------------------------------------------------------------

queuePresent :: WithPoked (TMaybe.M mn) => Queue.Q -> PresentInfo mn -> IO ()
queuePresent (Queue.Q q) pi_ =
	presentInfoMiddleToCore pi_ \cpi -> do
	withPoked cpi \ppi -> do
		r <- C.queuePresent q ppi
		let	(fromIntegral -> rc) = C.presentInfoSwapchainCount cpi
		rs <- peekArray rc $ C.presentInfoPResults cpi
		throwUnlessSuccesses $ Result <$> rs
		throwUnlessSuccess $ Result r

data PresentInfo mn = PresentInfo {
	presentInfoNext :: TMaybe.M mn,
	presentInfoWaitSemaphores :: [Semaphore.M.S],
	presentInfoSwapchainImageIndices ::
		[(S, Word32)] }

deriving instance Show (TMaybe.M mn) => Show (PresentInfo mn)

presentInfoMiddleToCore ::
	WithPoked (TMaybe.M mn) => PresentInfo mn -> (C.PresentInfo -> IO a) -> IO ()
presentInfoMiddleToCore PresentInfo {
	presentInfoNext = mnxt,
	presentInfoWaitSemaphores =
		(length &&& id) . (Semaphore.M.unS <$>) -> (wsc, wss),
	presentInfoSwapchainImageIndices =
		(length &&& id . unzip) -> (scc, (scs, iis)) } f =
	sToCore `mapM` scs >>= \scs' ->
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	allocaArray wsc \pwss ->
	pokeArray pwss wss >>
	allocaArray scc \pscs ->
	pokeArray pscs scs' >>
	allocaArray scc \piis ->
	pokeArray piis iis >>
	allocaArray scc \prs -> f C.PresentInfo {
		C.presentInfoSType = (),
		C.presentInfoPNext = pnxt',
		C.presentInfoWaitSemaphoreCount = fromIntegral wsc,
		C.presentInfoPWaitSemaphores = pwss,
		C.presentInfoSwapchainCount = fromIntegral scc,
		C.presentInfoPSwapchains = pscs,
		C.presentInfoPImageIndices = piis,
		C.presentInfoPResults = prs }
