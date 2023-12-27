{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Fence.Internal (

	-- * CREATE

	create, F(..), M.CreateInfo(..),

	-- ** Group

	group, Group, create', unsafeDestroy, lookup,

	-- * WAIT FOR FENCES AND RESET FENCES

	waitForFs, resetFs

	) where

import Prelude hiding (lookup)
import Foreign.Storable.PeekPoke
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Exception
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry
import Data.HeteroParList qualified as HeteroParList
import Data.Map qualified as Map
import Data.Word
import Data.Time

import Gpu.Vulkan.Fence.Type

import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Fence.Middle as M

create :: (WithPoked (TMaybe.M mn), AllocationCallbacks.ToMiddle mac) =>
	Device.D sd -> M.CreateInfo mn ->
	TPMaybe.M (U2 AllocationCallbacks.A) mac ->
	(forall s . F s -> IO a) -> IO a
create (Device.D dvc) ci (AllocationCallbacks.toMiddle -> mac) f =
	bracket (M.create dvc ci mac) (\fnc -> M.destroy dvc fnc mac) (f . F)

waitForFs :: Device.D sd -> HeteroParList.PL F sfs -> Bool -> Maybe DiffTime -> IO ()
waitForFs (Device.D dvc) fs wa (maybe maxBound diffTimeToNanoseconds -> to) =
	M.waitForFs dvc (HeteroParList.toList (\(F f) -> f) fs) wa to

resetFs :: Device.D sd -> HeteroParList.PL F sfs -> IO ()
resetFs (Device.D dvc) = M.resetFs dvc . HeteroParList.toList \(F f) -> f

diffTimeToNanoseconds :: DiffTime -> Word64
diffTimeToNanoseconds = fromInteger . (`div` 1000) . diffTimeToPicoseconds

data Group sd ma sf k = Group (Device.D sd)
	(TPMaybe.M (U2 AllocationCallbacks.A) ma) TSem (TVar (Map.Map k (F sf)))

group :: AllocationCallbacks.ToMiddle ma =>
	Device.D sd -> TPMaybe.M (U2 AllocationCallbacks.A) ma ->
	(forall sf . Group sd ma sf k -> IO a) -> IO a
group dvc@(Device.D mdvc) mac@(AllocationCallbacks.toMiddle -> mmac) f = do
	(sem, m) <- atomically $ (,) <$> newTSem 1 <*> newTVar Map.empty
	rtn <- f $ Group dvc mac sem m
	((\(F s) -> M.destroy mdvc s mmac) `mapM_`) =<< atomically (readTVar m)
	pure rtn

create' :: (
	Ord k, WithPoked (TMaybe.M mn), AllocationCallbacks.ToMiddle ma) =>
	Group sd ma sf k -> k -> M.CreateInfo mn -> IO (Either String (F sf))
create' (Group (Device.D mdvc)
	(AllocationCallbacks.toMiddle -> mmac) sem sf) k ci = do
	ok <- atomically do
		mx <- Map.lookup k <$> readTVar sf
		case mx of
			Nothing -> waitTSem sem >> pure True
			Just _ -> pure False
	if ok
	then do	f <- M.create mdvc ci mmac
		let	f' = F f
		atomically $ modifyTVar sf (Map.insert k f') >> signalTSem sem
		pure $ Right f'
	else pure . Left $
		"Gpu.Vulkan.Fence.Internal.create': The key already exist"

unsafeDestroy :: (
	Ord k, AllocationCallbacks.ToMiddle ma) =>
	Group sd ma sf k -> k -> IO (Either String ())
unsafeDestroy (Group (Device.D mdvc)
	(AllocationCallbacks.toMiddle -> ma) sem fs) k = do
	mf <- atomically do
		mx <- Map.lookup k <$> readTVar fs
		case mx of
			Nothing -> pure Nothing
			Just _ -> waitTSem sem >> pure mx
	case mf of
		Nothing -> pure $ Left
			"Gpu.Vulkan.Fence.unsafeDestroy: No such key"
		Just (F f) -> do
			M.destroy mdvc f ma
			atomically do
				modifyTVar fs $ Map.delete k
				signalTSem sem
				pure $ Right ()

lookup :: Ord k => Group sd ma sf k -> k -> IO (Maybe (F sf))
lookup (Group _ _ _sem fs) k = atomically $ Map.lookup k <$> readTVar fs
