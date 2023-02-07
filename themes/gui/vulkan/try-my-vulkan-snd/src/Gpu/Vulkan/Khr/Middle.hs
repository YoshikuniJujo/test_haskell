{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Middle (

	-- * Acquire Next Image

	acquireNextImage, acquireNextImageResult,

	-- * Queue Present

	queuePresent, PresentInfo(..) ) where

import Foreign.Marshal.Alloc
import Foreign.Storable
import Control.Monad.Cont
import Data.Word

import Gpu.Vulkan.Misc.Middle.Internal
import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum

import qualified Gpu.Vulkan.Device.Middle.Internal as Device.M
import qualified Gpu.Vulkan.Khr.Core as C
import qualified Gpu.Vulkan.Khr.Swapchain.Middle.Internal as Swapchain.M
import qualified Gpu.Vulkan.Fence.Middle.Internal as Fence
import qualified Gpu.Vulkan.Semaphore.Middle.Internal as Semaphore.M

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Storable.PeekPoke (WithPoked, withPokedMaybe', withPtrS)
import Control.Arrow
import Gpu.Vulkan.Queue.Middle.Internal as Queue

acquireNextImage :: Device.M.D ->
	Swapchain.M.S -> Word64 -> Maybe Semaphore.M.S -> Maybe Fence.F -> IO Word32
acquireNextImage = acquireNextImageResult [Success]

acquireNextImageResult :: [Result] -> Device.M.D ->
	Swapchain.M.S -> Word64 -> Maybe Semaphore.M.S -> Maybe Fence.F -> IO Word32
acquireNextImageResult sccs
	(Device.M.D dvc) sc to msmp mfnc = ($ pure) $ runContT do
	let	smp = maybe NullHandle (\(Semaphore.M.S s) -> s) msmp
		fnc = maybe NullHandle (\(Fence.F f) -> f) mfnc
	pii <- ContT alloca
	sc' <- lift $ Swapchain.M.sToCore sc
	lift do	r <- C.acquireNextImage dvc sc' to smp fnc pii
		throwUnless sccs $ Result r
		peek pii

---------------------------------------------------------------------------

queuePresent :: WithPoked n => Queue.Q -> PresentInfo n -> IO ()
queuePresent (Queue.Q q) pi_ = ($ pure) $ runContT do
	cpi@(C.PresentInfo_ fpi) <- ContT $ presentInfoMiddleToCore pi_
	ppi <- ContT $ withForeignPtr fpi
	lift do r <- C.queuePresent q ppi
		let	(fromIntegral -> rc) = C.presentInfoSwapchainCount cpi
		rs <- peekArray rc $ C.presentInfoPResults cpi
		throwUnlessSuccesses $ Result <$> rs
		throwUnlessSuccess $ Result r

data PresentInfo n = PresentInfo {
	presentInfoNext :: Maybe n,
	presentInfoWaitSemaphores :: [Semaphore.M.S],
	presentInfoSwapchainImageIndices ::
		[(Swapchain.M.S, Word32)]
	} deriving Show

presentInfoMiddleToCore ::
	WithPoked n => PresentInfo n -> (C.PresentInfo -> IO a) -> IO ()
presentInfoMiddleToCore PresentInfo {
	presentInfoNext = mnxt,
	presentInfoWaitSemaphores =
		(length &&& id) . (Semaphore.M.unS <$>) -> (wsc, wss),
	presentInfoSwapchainImageIndices =
		(length &&& id . unzip) -> (scc, (scs, iis)) } f =
	Swapchain.M.sToCore `mapM` scs >>= \scs' ->
	withPokedMaybe' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
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
