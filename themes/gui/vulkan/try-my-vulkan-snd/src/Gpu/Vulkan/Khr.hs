{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr where

import Foreign.Pointable
import Data.HeteroList
import Data.Word

import qualified Data.Text as T

import Gpu.Vulkan.Exception.Enum

import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Device.Middle as Device.M
import qualified Gpu.Vulkan.Semaphore as Semaphore
import qualified Gpu.Vulkan.Fence.Middle as Fence
import qualified Gpu.Vulkan.Queue as Queue
import qualified Gpu.Vulkan.Khr.Swapchain.Type as Swapchain
import qualified Gpu.Vulkan.Khr.Swapchain.Middle.Internal as Swapchain.M
import qualified Gpu.Vulkan.Khr.Middle as M

validationLayerName :: T.Text
validationLayerName = "VK_LAYER_KHRONOS_validation"

acquireNextImageResultNew :: [Result] -> Device.D sd ->
	Swapchain.SNew ssc scfmt -> Word64 -> Maybe (Semaphore.S ss) -> Maybe Fence.F -> IO Word32
acquireNextImageResultNew sccs dvc (Swapchain.sFromNew -> sc) to msmp mfnc =
	acquireNextImageResult sccs dvc sc to msmp mfnc

acquireNextImage :: Device.D sd ->
	Swapchain.S ssc -> Word64 -> Maybe (Semaphore.S ss) -> Maybe Fence.F -> IO Word32
acquireNextImage = acquireNextImageResult [Success]

acquireNextImageResult :: [Result] -> Device.D sd ->
	Swapchain.S ssc -> Word64 -> Maybe (Semaphore.S ss) -> Maybe Fence.F -> IO Word32
acquireNextImageResult sccs
	(Device.D dvc) sc to msmp mfnc = acquireNextImageResultM sccs dvc sc to msmp mfnc

data SwapchainImageIndexNew scfmt ssc =
	SwapchainImageIndexNew (Swapchain.SNew ssc scfmt) Word32 deriving Show

swapchainImageIndexFromNew ::
	SwapchainImageIndexNew scfmt ssc -> SwapchainImageIndex ssc
swapchainImageIndexFromNew (SwapchainImageIndexNew sc iix) =
	SwapchainImageIndex (Swapchain.sFromNew sc) iix

data SwapchainImageIndex ssc =
	SwapchainImageIndex (Swapchain.S ssc) Word32 deriving Show

swapchainImageIndexToMiddle ::
	SwapchainImageIndex ssc -> (Swapchain.M.S, Word32)
swapchainImageIndexToMiddle (SwapchainImageIndex (Swapchain.S sc) idx) =
	(sc, idx)

data PresentInfoNew n sws scfmt sscs = PresentInfoNew {
	presentInfoNextNew :: Maybe n,
	presentInfoWaitSemaphoresNew :: HeteroVarList Semaphore.S sws,
	presentInfoSwapchainImageIndicesNew ::
		HeteroVarList (SwapchainImageIndexNew scfmt) sscs }


presentInfoFromNew ::
	PresentInfoNew n sws scfmt sscs -> PresentInfo n sws sscs
presentInfoFromNew PresentInfoNew {
	presentInfoNextNew = mnxt,
	presentInfoWaitSemaphoresNew = wsmps,
	presentInfoSwapchainImageIndicesNew = sciis } = PresentInfo {
	presentInfoNext = mnxt,
	presentInfoWaitSemaphores = wsmps,
	presentInfoSwapchainImageIndices =
		heteroVarListMap swapchainImageIndexFromNew sciis }

data PresentInfo n sws sscs = PresentInfo {
	presentInfoNext :: Maybe n,
	presentInfoWaitSemaphores :: HeteroVarList Semaphore.S sws,
	presentInfoSwapchainImageIndices ::
		HeteroVarList SwapchainImageIndex sscs }

deriving instance (
	Show n, Show (HeteroVarList Semaphore.S sws),
	Show (HeteroVarList SwapchainImageIndex sscs)) =>
	Show (PresentInfo n sws sscs)

presentInfoFromMiddle :: PresentInfo n sws sccs -> M.PresentInfo n
presentInfoFromMiddle PresentInfo {
	presentInfoNext = mnxt,
	presentInfoWaitSemaphores =
		heteroVarListToList (\(Semaphore.S s) -> s) -> wss,
	presentInfoSwapchainImageIndices =
		heteroVarListToList swapchainImageIndexToMiddle -> sciis
	} = M.PresentInfo {
		M.presentInfoNext = mnxt,
		M.presentInfoWaitSemaphores = wss,
		M.presentInfoSwapchainImageIndices = sciis }

queuePresentNew ::
	Pointable n => Queue.Q -> PresentInfoNew n sws scfmt sscs -> IO ()
queuePresentNew q = queuePresent q . presentInfoFromNew

queuePresent :: Pointable n => Queue.Q -> PresentInfo n sws sscs -> IO ()
queuePresent q = M.queuePresent q . presentInfoFromMiddle

acquireNextImageResultNewM :: [Result] -> Device.M.D ->
	Swapchain.SNew ssc scfmt -> Word64 -> Maybe (Semaphore.S ss) -> Maybe Fence.F -> IO Word32
acquireNextImageResultNewM sccs dvc (Swapchain.sFromNew -> sc) to msmp mfnc =
	acquireNextImageResultM sccs dvc sc to msmp mfnc

acquireNextImageM :: Device.M.D ->
	Swapchain.S ssc -> Word64 -> Maybe (Semaphore.S ss) -> Maybe Fence.F -> IO Word32
acquireNextImageM = acquireNextImageResultM [Success]

acquireNextImageResultM :: [Result] -> Device.M.D -> Swapchain.S ssc ->
	Word64 -> Maybe (Semaphore.S ss) -> Maybe Fence.F -> IO Word32
acquireNextImageResultM sccs dvc (Swapchain.S sc) to msmp mfnc =
	M.acquireNextImageResult
		sccs dvc sc to ((\(Semaphore.S smp) -> smp) <$> msmp) mfnc
