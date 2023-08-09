{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device.Internal (

	-- * CREATE

	create, D(..), M.CreateInfo(..), M.CreateFlags, M.QueueCreateInfo(..),

	-- * GET QUEUE AND WAIT IDLE

	getQueue, waitIdle,

	-- * SIZE

	M.Size

	) where

import Foreign.Storable.PeekPoke
import Control.Exception
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry

import Gpu.Vulkan.Device.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.PhysicalDevice as PhysicalDevice
import qualified Gpu.Vulkan.Device.Middle as M
import qualified Gpu.Vulkan.QueueFamily.Middle as QueueFamily
import qualified Gpu.Vulkan.Queue as Queue

import Data.HeteroParList qualified as HeteroParList

create :: (
	WithPoked (TMaybe.M mn),
	HeteroParList.ToListWithCM' WithPoked TMaybe.M qcis,
	AllocationCallbacks.ToMiddle mac ) =>
	PhysicalDevice.P -> M.CreateInfo mn qcis ->
	TPMaybe.M (U2 AllocationCallbacks.A) mac ->
	(forall s . D s -> IO a) -> IO a
create pd ci (AllocationCallbacks.toMiddle -> mac) f =
	bracket (M.create pd ci mac) (`M.destroy` mac) (f . D)

getQueue :: D sd -> QueueFamily.Index -> Queue.Index -> IO Queue.Q
getQueue (D d) (QueueFamily.Index qfi) qi = M.getQueue d qfi qi

waitIdle :: D s -> IO ()
waitIdle (D d) = M.waitIdle d
