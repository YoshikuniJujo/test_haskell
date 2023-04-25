{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr where

import Foreign.Storable.PeekPoke
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.HeteroParList qualified as HeteroParList
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

data PresentInfoNew mn sws scfmt sscs = PresentInfoNew {
	presentInfoNextNew :: TMaybe.M mn,
	presentInfoWaitSemaphoresNew :: HeteroParList.PL Semaphore.S sws,
	presentInfoSwapchainImageIndicesNew ::
		HeteroParList.PL (SwapchainImageIndexNew scfmt) sscs }


presentInfoFromNew ::
	PresentInfoNew mn sws scfmt sscs -> PresentInfo mn sws sscs
presentInfoFromNew PresentInfoNew {
	presentInfoNextNew = mnxt,
	presentInfoWaitSemaphoresNew = wsmps,
	presentInfoSwapchainImageIndicesNew = sciis } = PresentInfo {
	presentInfoNext = mnxt,
	presentInfoWaitSemaphores = wsmps,
	presentInfoSwapchainImageIndices =
		HeteroParList.map swapchainImageIndexFromNew sciis }

data PresentInfo mn sws sscs = PresentInfo {
	presentInfoNext :: TMaybe.M mn,
	presentInfoWaitSemaphores :: HeteroParList.PL Semaphore.S sws,
	presentInfoSwapchainImageIndices ::
		HeteroParList.PL SwapchainImageIndex sscs }

deriving instance (
	Show (TMaybe.M mn), Show (HeteroParList.PL Semaphore.S sws),
	Show (HeteroParList.PL SwapchainImageIndex sscs)) =>
	Show (PresentInfo mn sws sscs)

presentInfoFromMiddle :: PresentInfo n sws sccs -> M.PresentInfo n
presentInfoFromMiddle PresentInfo {
	presentInfoNext = mnxt,
	presentInfoWaitSemaphores =
		HeteroParList.toList (\(Semaphore.S s) -> s) -> wss,
	presentInfoSwapchainImageIndices =
		HeteroParList.toList swapchainImageIndexToMiddle -> sciis
	} = M.PresentInfo {
		M.presentInfoNext = mnxt,
		M.presentInfoWaitSemaphores = wss,
		M.presentInfoSwapchainImageIndices = sciis }

queuePresentNew ::
	WithPoked (TMaybe.M mn) => Queue.Q -> PresentInfoNew mn sws scfmt sscs -> IO ()
queuePresentNew q = queuePresent q . presentInfoFromNew

queuePresent :: WithPoked (TMaybe.M mn) => Queue.Q -> PresentInfo mn sws sscs -> IO ()
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
