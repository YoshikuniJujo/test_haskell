{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Surface.Internal (
	S(..), group, destroy, lookup, Group(..),

	M.Capabilities(..), M.Format(..)) where

import Prelude hiding (lookup)
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry
import Data.Map qualified as Map

import Gpu.Vulkan.Instance.Internal qualified as Instance
import Gpu.Vulkan.AllocationCallbacks.Internal qualified as AllocationCallbacks
import Gpu.Vulkan.Khr.Surface.Type
import Gpu.Vulkan.Khr.Surface.Middle qualified as M

data Group ma s k = Group (TPMaybe.M (U2 AllocationCallbacks.A) ma)
	TSem (TVar (Map.Map k (S s)))

group :: AllocationCallbacks.ToMiddle ma =>
	Instance.I si -> TPMaybe.M (U2 AllocationCallbacks.A) ma ->
	(forall s . Group ma s k -> IO a) -> IO a
group (Instance.I mi) mac@(AllocationCallbacks.toMiddle -> ma) f = do
	(sem, m) <- atomically $ (,) <$> newTSem 1 <*> newTVar Map.empty
	rtn <- f $ Group mac sem m
	((\(S s) -> M.destroy mi s ma) `mapM_`) =<< atomically (readTVar m)
	pure rtn

destroy :: (Ord k, AllocationCallbacks.ToMiddle ma) =>
	Instance.I si -> Group ma s k -> k -> IO (Either String ())
destroy (Instance.I mi) (Group (AllocationCallbacks.toMiddle -> ma) sem ss) k =
	do	mbs <- atomically do
			mx <- Map.lookup k <$> readTVar ss
			case mx of
				Nothing -> pure Nothing
				Just _ -> waitTSem sem >> pure mx
		case mbs of
			Nothing -> pure $ Left
				"Gpu.Vulkan.Khr.Surface.Internal: No such key"
			Just (S ms) -> do
				M.destroy mi ms ma
				atomically do
					modifyTVar ss (Map.delete k)
					signalTSem sem
					pure $ Right ()

lookup :: Ord k => Group ma s k -> k -> IO (Maybe (S s))
lookup (Group _ _sem ss) k = atomically $ Map.lookup k <$> readTVar ss
