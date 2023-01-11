{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Middle where

import Foreign.Marshal.Alloc
import Foreign.Storable
import Control.Monad.Cont
import Data.Word

import Gpu.Vulkan.Misc
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
import Foreign.Pointable
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

queuePresentMiddle :: Pointable n => Queue.Q -> PresentInfoMiddle n -> IO ()
queuePresentMiddle (Queue.Q q) pi_ = ($ pure) $ runContT do
	cpi@(C.PresentInfo_ fpi) <- presentInfoMiddleToCore pi_
	ppi <- ContT $ withForeignPtr fpi
	lift do r <- C.queuePresent q ppi
		let	(fromIntegral -> rc) = C.presentInfoSwapchainCount cpi
		rs <- peekArray rc $ C.presentInfoPResults cpi
		throwUnlessSuccesses $ Result <$> rs
		throwUnlessSuccess $ Result r

data PresentInfoMiddle n = PresentInfoMiddle {
	presentInfoNextMiddle :: Maybe n,
	presentInfoWaitSemaphoresMiddle :: [Semaphore.M.S],
	presentInfoSwapchainImageIndicesMiddle ::
		[(Swapchain.M.S, Word32)]
	} deriving Show

presentInfoMiddleToCore ::
	Pointable n => PresentInfoMiddle n -> ContT r IO C.PresentInfo
presentInfoMiddleToCore PresentInfoMiddle {
	presentInfoNextMiddle = mnxt,
	presentInfoWaitSemaphoresMiddle =
		(length &&& id) . (Semaphore.M.unS <$>) -> (wsc, wss),
	presentInfoSwapchainImageIndicesMiddle =
		(length &&& id . unzip) -> (scc, (scs, iis)) } = do
	scs' <- lift $ Swapchain.M.sToCore `mapM` scs
	(castPtr -> pnxt) <- maybeToPointer mnxt
	pwss <- ContT $ allocaArray wsc
	lift $ pokeArray pwss wss
	pscs <- ContT $ allocaArray scc
	lift $ pokeArray pscs scs'
	piis <- ContT $ allocaArray scc
	lift $ pokeArray piis iis
	prs <- ContT $ allocaArray scc
	pure C.PresentInfo {
		C.presentInfoSType = (),
		C.presentInfoPNext = pnxt,
		C.presentInfoWaitSemaphoreCount = fromIntegral wsc,
		C.presentInfoPWaitSemaphores = pwss,
		C.presentInfoSwapchainCount = fromIntegral scc,
		C.presentInfoPSwapchains = pscs,
		C.presentInfoPImageIndices = piis,
		C.presentInfoPResults = prs }
