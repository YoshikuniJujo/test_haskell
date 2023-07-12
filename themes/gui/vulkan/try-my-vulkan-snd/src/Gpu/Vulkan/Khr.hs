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

	queuePresentNew, PresentInfoNew(..), SwapchainImageIndexNew(..),

	-- * ACQUIRE NEXT IMAGE

	acquireNextImageNew, acquireNextImageResultNew,

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

queuePresentNew :: WithPoked (TMaybe.M mn) =>
	Queue.Q -> PresentInfoNew mn sws scfmt sscs -> IO ()
queuePresentNew q = M.queuePresent q . presentInfoToMiddle

data PresentInfoNew mn sws scfmt sscs = PresentInfoNew {
	presentInfoNextNew :: TMaybe.M mn,
	presentInfoWaitSemaphoresNew :: HeteroParList.PL Semaphore.S sws,
	presentInfoSwapchainImageIndicesNew ::
		HeteroParList.PL (SwapchainImageIndexNew scfmt) sscs }

presentInfoToMiddle :: PresentInfoNew mn sws scfmt sscs -> M.PresentInfo mn
presentInfoToMiddle PresentInfoNew {
	presentInfoNextNew = mnxt,
	presentInfoWaitSemaphoresNew =
		HeteroParList.toList (\(Semaphore.S s) -> s) -> wss,
	presentInfoSwapchainImageIndicesNew =
		HeteroParList.toList swapchainImageIndexToMiddle -> sciis
	} = M.PresentInfo {
		M.presentInfoNext = mnxt,
		M.presentInfoWaitSemaphores = wss,
		M.presentInfoSwapchainImageIndices = sciis }

data SwapchainImageIndexNew scfmt ssc =
	SwapchainImageIndexNew (Swapchain.SNew ssc scfmt) Word32 deriving Show

swapchainImageIndexToMiddle ::
	SwapchainImageIndexNew scfmt ssc -> (Swapchain.M.S, Word32)
swapchainImageIndexToMiddle (SwapchainImageIndexNew (Swapchain.SNew sc) idx) =
	(sc, idx)

acquireNextImageNew :: Device.D sd ->
	Swapchain.SNew ssc scfmt -> Word64 -> Maybe (Semaphore.S ss) -> Maybe Fence.F -> IO Word32
acquireNextImageNew = acquireNextImageResultNew [Success]

acquireNextImageResultNew :: [Result] -> Device.D sd ->
	Swapchain.SNew ssc scfmt -> Word64 -> Maybe (Semaphore.S ss) -> Maybe Fence.F -> IO Word32
acquireNextImageResultNew sccs (Device.D mdvc) (Swapchain.SNew msc) to msmp mfnc =
	M.acquireNextImageResult
		sccs mdvc msc to ((\(Semaphore.S smp) -> smp) <$> msmp) mfnc
