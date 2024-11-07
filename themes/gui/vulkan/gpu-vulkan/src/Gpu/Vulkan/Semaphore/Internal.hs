{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Semaphore.Internal (

	-- * CREATE

	create, S(..), M.CreateInfo(..),

	-- ** Group

	group, Group, create', unsafeDestroy, lookup,

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
import Data.Word

import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Semaphore.Middle as M

import Gpu.Vulkan.Semaphore.Type

create :: (WithPoked (TMaybe.M mn), AllocationCallbacks.ToMiddle mac) =>
	Device.D sd -> M.CreateInfo mn ->
	TPMaybe.M (U2 AllocationCallbacks.A) mac ->
	(forall ss . S ss -> IO a) -> IO a
create (Device.D dvc) ci
	(AllocationCallbacks.toMiddle -> macc) f = bracket
	(M.create dvc ci macc) (\s -> M.destroy dvc s macc) (f . S)

data Group sd ma ss k = Group (Device.D sd)
	(TPMaybe.M (U2 AllocationCallbacks.A) ma) TSem (TVar (Map.Map k (S ss)))

group :: AllocationCallbacks.ToMiddle ma =>
	Device.D sd -> TPMaybe.M (U2 AllocationCallbacks.A) ma ->
	(forall ss . Group sd ma ss k -> IO a) -> IO a
group dvc@(Device.D mdvc) mac@(AllocationCallbacks.toMiddle -> mmac) f = do
	(sem, m) <- atomically $ (,) <$> newTSem 1 <*> newTVar Map.empty
	rtn <- f $ Group dvc mac sem m
	((\(S s) -> M.destroy mdvc s mmac) `mapM_`) =<< atomically (readTVar m)
	pure rtn

create' :: (
	Ord k, WithPoked (TMaybe.M mn), AllocationCallbacks.ToMiddle ma) =>
	Group sd ma ss k -> k -> M.CreateInfo mn -> IO (Either String (S ss))
create' (Group (Device.D mdvc)
	(AllocationCallbacks.toMiddle -> mmac) sem ss) k ci = do
	ok <- atomically do
		mx <- Map.lookup k <$> readTVar ss
		case mx of
			Nothing -> waitTSem sem >> pure True
			Just _ -> pure False
	if ok
	then do	s <- M.create mdvc ci mmac
		let	s' = S s
		atomically $ modifyTVar ss (Map.insert k s') >> signalTSem sem
		pure $ Right s'
	else pure . Left $
		"Gpu.Vulkan.Semaphore.Internal.create': The key already exist"

unsafeDestroy :: (Ord k, AllocationCallbacks.ToMiddle ma) =>
	Group sd ma ss k -> k -> IO (Either String ())
unsafeDestroy (Group (Device.D mdvc)
	(AllocationCallbacks.toMiddle -> ma) sem ss) k = do
	ms <- atomically do
		mx <- Map.lookup k <$> readTVar ss
		case mx of
			Nothing -> pure Nothing
			Just _ -> waitTSem sem >> pure mx
	case ms of
		Nothing -> pure $ Left
			"Gpu.Vulkan.Semaphore.unsafeDestroy: No such key"
		Just (S s) -> do
			M.destroy mdvc s ma
			atomically do
				modifyTVar ss $ Map.delete k
				signalTSem sem
				pure $ Right ()

lookup :: Ord k => Group sd ma ss k -> k -> IO (Maybe (S ss))
lookup (Group _ _ _sem ss) k = atomically $ Map.lookup k <$> readTVar ss
