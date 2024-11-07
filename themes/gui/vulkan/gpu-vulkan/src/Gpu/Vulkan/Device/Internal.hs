{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
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

	-- ** Group

	group, Group, create', unsafeDestroy, lookup,

	-- * GET QUEUE AND WAIT IDLE

	getQueue, waitIdle,

	-- * SIZE

	M.Size

	) where

import Prelude hiding (lookup)
import Foreign.Storable.PeekPoke
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Exception
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry
import Data.Map qualified as Map

import Gpu.Vulkan
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

data Group ma sd k = Group
	(TPMaybe.M (U2 AllocationCallbacks.A) ma) TSem (TVar (Map.Map k (D sd)))

group :: AllocationCallbacks.ToMiddle ma =>
	TPMaybe.M (U2 AllocationCallbacks.A) ma ->
	(forall sd . Group ma sd k -> IO a) -> IO a
group mac@(AllocationCallbacks.toMiddle -> mmac) f = do
	(sem, m) <- atomically $ (,) <$> newTSem 1 <*> newTVar Map.empty
	rtn <- f $ Group mac sem m
	((\(D d) -> M.destroy d mmac) `mapM_`) =<< atomically (readTVar m)
	pure rtn

create' :: (
	Ord k, WithPoked (TMaybe.M mn),
	HeteroParList.ToListWithCM' WithPoked TMaybe.M qcis,
	AllocationCallbacks.ToMiddle ma ) => PhysicalDevice.P ->
	Group ma sd k -> k -> CreateInfo mn qcis -> IO (Either String (D sd))
create' phd (Group
	(AllocationCallbacks.toMiddle -> mmac) sem ds) k
	(createInfoToMiddle -> ci) = do
	ok <- atomically do
		mx <- Map.lookup k <$> readTVar ds
		case mx of
			Nothing -> waitTSem sem >> pure True
			Just _ -> pure False
	if ok
	then do	d <- M.create phd ci mmac
		let	d' = D d
		atomically $ modifyTVar ds (Map.insert k d') >> signalTSem sem
		pure $ Right d'
	else pure . Left $ "Gpu.Vulkan.Device.create': The key already exist"

unsafeDestroy :: (Ord k, AllocationCallbacks.ToMiddle ma) =>
	Group ma sd k -> k -> IO (Either String ())
unsafeDestroy (Group
	(AllocationCallbacks.toMiddle -> ma) sem ds) k = do
	md <- atomically do
		mx <- Map.lookup k <$> readTVar ds
		case mx of
			Nothing -> pure Nothing
			Just _ -> waitTSem sem >> pure mx
	case md of
		Nothing -> pure $ Left "Gpu.Vulkan.Device.unsafaDestroy: No such key"
		Just (D d) -> do
			M.destroy d ma
			atomically do
				modifyTVar ds $ Map.delete k
				signalTSem sem
				pure $ Right ()

lookup :: Ord k => Group ma sd k -> k -> IO (Maybe (D sd))
lookup (Group _ _sem ds) k = atomically $ Map.lookup k <$> readTVar ds
