{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Surface.Glfw.Window (

	-- * CREATE AND DESTROY

	W, Group, group, create', destroy, lookup,

	shouldClose,

	-- ** HINT

	hint, B.WindowHint(..),
	B.ClientAPI(..),

	-- * SURFACE

	createSurface,

	-- * CALLBACK

	setFramebufferSizeCallback, B.FramebufferSizeCallback,

	-- * PARAMETER

	getFramebufferSize

	) where

import Prelude hiding (lookup)

import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Exception
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry
import Data.Map qualified as M
import Graphics.UI.GLFW qualified as B

import Gpu.Vulkan.AllocationCallbacks.Internal qualified as AllocationCallbacks
import Gpu.Vulkan.Instance.Internal qualified as Vk.Instance
import Gpu.Vulkan.Khr.Surface.Type qualified as Vk.Khr.Surface
import Gpu.Vulkan.Khr.Surface.Middle qualified as Vk.Khr.Surface.M
import Gpu.Vulkan.Khr.Surface.Glfw.Middle qualified as M
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

hint :: B.WindowHint -> IO ()
hint = B.windowHint

lookup :: Ord k => Group s k -> k -> IO (Maybe (W s))
lookup (Group _sem ws) k = atomically $ M.lookup k <$> readTVar ws

setFramebufferSizeCallback :: W s -> Maybe B.FramebufferSizeCallback -> IO ()
setFramebufferSizeCallback (W w) = B.setFramebufferSizeCallback w

createSurface :: (AllocationCallbacks.ToMiddle mscc ) =>
	Vk.Instance.I si -> W s ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc ->
	(forall ss . Vk.Khr.Surface.S ss -> IO a) -> IO a
createSurface (Vk.Instance.I ist) (W win)
	(AllocationCallbacks.toMiddle -> macc) f = bracket
	(M.createWindowSurface ist win macc)
	(\sfc -> Vk.Khr.Surface.M.destroy ist sfc macc)
	(f . Vk.Khr.Surface.S)

getFramebufferSize :: W sw -> IO (Int, Int)
getFramebufferSize (W w) = B.getFramebufferSize w

shouldClose :: W sw -> IO Bool
shouldClose (W w) = B.windowShouldClose w
