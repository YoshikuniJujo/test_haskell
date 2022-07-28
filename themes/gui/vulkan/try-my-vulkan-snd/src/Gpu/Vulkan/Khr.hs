{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Data.Word

import qualified Data.Text as T

import Gpu.Vulkan.Base
import Gpu.Vulkan.Exception
import Gpu.Vulkan.Exception.Enum

import qualified Gpu.Vulkan.Device.Middle as Device
import qualified Gpu.Vulkan.Semaphore.Middle as Semaphore
import qualified Gpu.Vulkan.Fence.Middle as Fence
import qualified Gpu.Vulkan.Queue as Queue
import qualified Gpu.Vulkan.Khr.Swapchain.Middle as Swapchain
import qualified Gpu.Vulkan.Khr.Core as C

validationLayerName :: T.Text
validationLayerName = "VK_LAYER_KHRONOS_validation"

acquireNextImage :: Device.D ->
	Swapchain.S -> Word64 -> Maybe Semaphore.S -> Maybe Fence.F -> IO Word32
acquireNextImage = acquireNextImageResult [Success]

acquireNextImageResult :: [Result] -> Device.D ->
	Swapchain.S -> Word64 -> Maybe Semaphore.S -> Maybe Fence.F -> IO Word32
acquireNextImageResult sccs
	(Device.D dvc) sc to msmp mfnc = ($ pure) $ runContT do
	let	smp = maybe NullHandle (\(Semaphore.S s) -> s) msmp
		fnc = maybe NullHandle (\(Fence.F f) -> f) mfnc
	pii <- ContT alloca
	sc' <- lift $ Swapchain.sToCore sc
	lift do	r <- C.acquireNextImage dvc sc' to smp fnc pii
		throwUnless sccs $ Result r
		peek pii

data PresentInfo n = PresentInfo {
	presentInfoNext :: Maybe n,
	presentInfoWaitSemaphores :: [Semaphore.S],
	presentInfoSwapchainImageIndices :: [(Swapchain.S, Word32)] }
	deriving Show

presentInfoToCore :: Pointable n => PresentInfo n -> ContT r IO C.PresentInfo
presentInfoToCore PresentInfo {
	presentInfoNext = mnxt,
	presentInfoWaitSemaphores =
		length &&& (Semaphore.unS <$>) -> (wsc, wss),
	presentInfoSwapchainImageIndices =
		length &&& id . unzip ->
		(scc, (scs, iis))
	} = do
	scs' <- lift $ Swapchain.sToCore `mapM` scs
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

queuePresent :: Pointable n => Queue.Q -> PresentInfo n -> IO ()
queuePresent (Queue.Q q) pi_ = ($ pure) $ runContT do
	cpi@(C.PresentInfo_ fpi) <- presentInfoToCore pi_
	ppi <- ContT $ withForeignPtr fpi
	lift do r <- C.queuePresent q ppi
		let	(fromIntegral -> rc) = C.presentInfoSwapchainCount cpi
		rs <- peekArray rc $ C.presentInfoPResults cpi
		throwUnlessSuccesses $ Result <$> rs
		throwUnlessSuccess $ Result r
