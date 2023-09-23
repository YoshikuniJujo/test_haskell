{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Surface.Glfw.Window (
	W, Group, group, create', destroy, lookup ) where

import Prelude hiding (lookup)

import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Data.Map qualified as M
import Graphics.UI.GLFW qualified as B
import Gpu.Vulkan.Khr.Surface.Glfw.Window.Type

data Group s k = Group TSem (TVar (M.Map k (W s)))

group :: (forall s . Group s k -> IO a) -> IO a
group f = do
	(sem, m) <- atomically $ (,) <$> newTSem 1 <*> newTVar M.empty
	rtn <- f $ Group sem m
	((\(W w) -> B.destroyWindow w) `mapM_`) =<< atomically (readTVar m)
	pure rtn

create' :: Ord k => Group s k -> k -> Int -> Int -> String ->
	Maybe B.Monitor -> Maybe B.Window -> IO (Either String (W s))
create' (Group sem ws) k wd hg ttl mm mws = do
	ok <- atomically do
		mx <- M.lookup k <$> readTVar ws
		case mx of
			Nothing -> waitTSem sem >> pure True
			Just _ -> pure False
	if ok
	then do	mw <- (W <$>) <$> B.createWindow wd hg ttl mm mws
		case mw of
			Just w -> atomically do
				modifyTVar ws (M.insert k w) 
				signalTSem sem
				pure $ Right w
			Nothing -> pure . Left $
				"Gpu.Vulkan.Khr.Surface." ++
				"Glfw.Window.create': GLFW-b: error"
	else pure . Left $ "Gpu.Vulkan.Khr.Surface.Glfw.Window.create': " ++
		"The key already exist"

destroy :: Ord k => Group s k -> k -> IO (Either String ())
destroy (Group sem ws) k = do
	mw <- atomically do
		mx <- M.lookup k <$> readTVar ws
		case mx of
			Nothing -> pure Nothing
			Just _ -> waitTSem sem >> pure mx
	case mw of
		Nothing -> pure . Left $
			"Gpu.Vulkan.Khr.Surface.Glfw.Window.destroy: " ++
			"No such key"
		Just (W w) -> do
			B.destroyWindow w
			atomically do
				modifyTVar ws (M.delete k)
				signalTSem sem
				pure $ Right ()

lookup :: Ord k => Group s k -> k -> IO (Maybe (W s))
lookup (Group _sem ws) k = atomically $ M.lookup k <$> readTVar ws
