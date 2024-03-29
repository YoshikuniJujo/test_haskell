{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr (

	-- * QUEUE PRESENT

	queuePresent, PresentInfo(..), SwapchainImageIndex(..),

	-- * ACQUIRE NEXT IMAGE

	acquireNextImage, acquireNextImageResult,

	-- * ENUM

	module Gpu.Vulkan.Khr.Enum

	) where

import Foreign.Storable.PeekPoke
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.HeteroParList qualified as HeteroParList
import Data.Word

import Gpu.Vulkan.Exception

import qualified Gpu.Vulkan.Device.Internal as Device
import qualified Gpu.Vulkan.Semaphore.Internal as Semaphore
import qualified Gpu.Vulkan.Fence.Internal as Fence
import qualified Gpu.Vulkan.Queue as Queue
import qualified Gpu.Vulkan.Khr.Swapchain.Type as Swapchain
import qualified Gpu.Vulkan.Khr.Swapchain.Middle.Internal as Swapchain.M
import qualified Gpu.Vulkan.Khr.Middle as M
import Gpu.Vulkan.Khr.Enum

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
	Swapchain.S scfmt ssc -> Word64 -> Maybe (Semaphore.S ss) -> Maybe (Fence.F sf) -> IO Word32
acquireNextImage = acquireNextImageResult [Success]

acquireNextImageResult :: [Result] -> Device.D sd ->
	Swapchain.S scfmt ssc -> Word64 -> Maybe (Semaphore.S ss) -> Maybe (Fence.F sf) -> IO Word32
acquireNextImageResult sccs (Device.D mdvc) (Swapchain.S msc) to msmp (((\(Fence.F f) -> f) <$>) -> mfnc) =
	M.acquireNextImageResult
		sccs mdvc msc to ((\(Semaphore.S smp) -> smp) <$> msmp) mfnc
