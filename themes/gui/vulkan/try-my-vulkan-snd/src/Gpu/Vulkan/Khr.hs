{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr (

	-- * LAYER NAME

	validationLayerName,

	-- * QUEUE PRESENT

	queuePresent, PresentInfo(..), SwapchainImageIndex(..),

	-- * ACQUIRE NEXT IMAGE

	acquireNextImage, acquireNextImageResult,

	) where

import Foreign.Storable.PeekPoke
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.HeteroParList qualified as HeteroParList
import Data.Word

import qualified Data.Text as T

import Gpu.Vulkan.Exception.Enum

import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Semaphore as Semaphore
import qualified Gpu.Vulkan.Fence.Middle as Fence
import qualified Gpu.Vulkan.Queue as Queue
import qualified Gpu.Vulkan.Khr.Swapchain.Type as Swapchain
import qualified Gpu.Vulkan.Khr.Swapchain.Middle.Internal as Swapchain.M
import qualified Gpu.Vulkan.Khr.Middle as M

validationLayerName :: T.Text
validationLayerName = "VK_LAYER_KHRONOS_validation"

queuePresent :: WithPoked (TMaybe.M mn) =>
	Queue.Q -> PresentInfo mn swss scfmt sscs -> IO ()
queuePresent q = M.queuePresent q . presentInfoToMiddle

data PresentInfo mn swss scfmt sscs = PresentInfo {
	presentInfoNext :: TMaybe.M mn,
	presentInfoWaitSemaphores :: HeteroParList.PL Semaphore.S swss,
	presentInfoSwapchainImageIndices ::
		HeteroParList.PL (SwapchainImageIndex scfmt) sscs }

presentInfoToMiddle :: PresentInfo mn sws scfmt sscs -> M.PresentInfo mn
presentInfoToMiddle PresentInfo {
	presentInfoNext = mnxt,
	presentInfoWaitSemaphores =
		HeteroParList.toList (\(Semaphore.S s) -> s) -> wss,
	presentInfoSwapchainImageIndices =
		HeteroParList.toList swapchainImageIndexToMiddle -> sciis
	} = M.PresentInfo {
		M.presentInfoNext = mnxt,
		M.presentInfoWaitSemaphores = wss,
		M.presentInfoSwapchainImageIndices = sciis }

data SwapchainImageIndex scfmt ssc =
	SwapchainImageIndex (Swapchain.S scfmt ssc) Word32 deriving Show

swapchainImageIndexToMiddle ::
	SwapchainImageIndex scfmt ssc -> (Swapchain.M.S, Word32)
swapchainImageIndexToMiddle (SwapchainImageIndex (Swapchain.S sc) idx) =
	(sc, idx)

acquireNextImage :: Device.D sd ->
	Swapchain.S scfmt ssc -> Word64 -> Maybe (Semaphore.S ss) -> Maybe Fence.F -> IO Word32
acquireNextImage = acquireNextImageResult [Success]

acquireNextImageResult :: [Result] -> Device.D sd ->
	Swapchain.S scfmt ssc -> Word64 -> Maybe (Semaphore.S ss) -> Maybe Fence.F -> IO Word32
acquireNextImageResult sccs (Device.D mdvc) (Swapchain.S msc) to msmp mfnc =
	M.acquireNextImageResult
		sccs mdvc msc to ((\(Semaphore.S smp) -> smp) <$> msmp) mfnc
