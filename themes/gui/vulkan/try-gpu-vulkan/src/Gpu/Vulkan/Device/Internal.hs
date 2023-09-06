{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device.Internal (

	-- * CREATE

	create, D(..), CreateInfo(..),
	M.CreateFlags, M.QueueCreateInfo(..),

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

import Gpu.Vulkan
import Gpu.Vulkan.Device.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.PhysicalDevice as PhysicalDevice
import qualified Gpu.Vulkan.PhysicalDevice.Struct as PhysicalDevice
import qualified Gpu.Vulkan.Device.Middle as M
import qualified Gpu.Vulkan.QueueFamily.Middle as QueueFamily
import qualified Gpu.Vulkan.Queue as Queue

import Data.HeteroParList qualified as HeteroParList

create :: (
	WithPoked (TMaybe.M mn),
	HeteroParList.ToListWithCM' WithPoked TMaybe.M qcis,
	AllocationCallbacks.ToMiddle mac ) =>
	PhysicalDevice.P -> CreateInfo mn qcis ->
	TPMaybe.M (U2 AllocationCallbacks.A) mac ->
	(forall s . D s -> IO a) -> IO a
create pd (createInfoToMiddle -> ci) (AllocationCallbacks.toMiddle -> mac) f =
	bracket (M.create pd ci mac) (`M.destroy` mac) (f . D)

getQueue :: D sd -> QueueFamily.Index -> Queue.Index -> IO Queue.Q
getQueue (D d) (QueueFamily.Index qfi) qi = M.getQueue d qfi qi

waitIdle :: D s -> IO ()
waitIdle (D d) = M.waitIdle d

data CreateInfo mn qcis = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: M.CreateFlags,
	createInfoQueueCreateInfos :: HeteroParList.PL M.QueueCreateInfo qcis,
	createInfoEnabledLayerNames :: [LayerName],
	createInfoEnabledExtensionNames :: [PhysicalDevice.ExtensionName],
	createInfoEnabledFeatures :: Maybe PhysicalDevice.Features }

deriving instance (
	Show (TMaybe.M mn), Show (HeteroParList.PL M.QueueCreateInfo qcis) ) =>
	Show (CreateInfo mn qcis)

createInfoToMiddle :: CreateInfo mn qcis -> M.CreateInfo mn qcis
createInfoToMiddle CreateInfo {
	createInfoNext = nxt,
	createInfoFlags = flgs,
	createInfoQueueCreateInfos = qcis,
	createInfoEnabledLayerNames = ((\(LayerName ln) -> ln) <$>) -> elnms,
	createInfoEnabledExtensionNames =
		((\(PhysicalDevice.ExtensionName en) -> en) <$>) ->eenms,
	createInfoEnabledFeatures = mef } = M.CreateInfo {
	M.createInfoNext = nxt,
	M.createInfoFlags = flgs,
	M.createInfoQueueCreateInfos = qcis,
	M.createInfoEnabledLayerNames = elnms,
	M.createInfoEnabledExtensionNames = eenms,
	M.createInfoEnabledFeatures = mef }
