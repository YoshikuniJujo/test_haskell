{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Middle (

	-- * Acquire Next Image

	acquireNextImage, acquireNextImageResult,

	-- * Queue Present

	queuePresent, PresentInfo(..) ) where

import Foreign.Marshal.Alloc
import Foreign.Storable
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.Word

import Gpu.Vulkan.Misc.Middle.Internal
import Gpu.Vulkan.Exception.Middle
import Gpu.Vulkan.Exception.Enum

import qualified Gpu.Vulkan.Device.Middle.Internal as Device.M
import qualified Gpu.Vulkan.Khr.Core as C
import qualified Gpu.Vulkan.Khr.Swapchain.Middle.Internal as Swapchain.M
import qualified Gpu.Vulkan.Fence.Middle.Internal as Fence
import qualified Gpu.Vulkan.Semaphore.Middle.Internal as Semaphore.M

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable.PeekPoke (WithPoked, withPoked, withPoked', withPtrS)
import Control.Arrow
import Gpu.Vulkan.Queue.Middle.Internal as Queue

acquireNextImage :: Device.M.D ->
	Swapchain.M.S -> Word64 -> Maybe Semaphore.M.S -> Maybe Fence.F -> IO Word32
acquireNextImage = acquireNextImageResult [Success]

acquireNextImageResult :: [Result] -> Device.M.D ->
	Swapchain.M.S -> Word64 -> Maybe Semaphore.M.S -> Maybe Fence.F -> IO Word32
acquireNextImageResult sccs
	(Device.M.D dvc) sc to msmp mfnc = alloca \pii ->
	Swapchain.M.sToCore sc >>= \sc' -> do
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
		[(Swapchain.M.S, Word32)] }

deriving instance Show (TMaybe.M mn) => Show (PresentInfo mn)

presentInfoMiddleToCore ::
	WithPoked (TMaybe.M mn) => PresentInfo mn -> (C.PresentInfo -> IO a) -> IO ()
presentInfoMiddleToCore PresentInfo {
	presentInfoNext = mnxt,
	presentInfoWaitSemaphores =
		(length &&& id) . (Semaphore.M.unS <$>) -> (wsc, wss),
	presentInfoSwapchainImageIndices =
		(length &&& id . unzip) -> (scc, (scs, iis)) } f =
	Swapchain.M.sToCore `mapM` scs >>= \scs' ->
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
