{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.UI.GlfwG.Window (

	-- * CREATE AND DESTROY

	W, create, Group, group, create', unsafeDestroy, lookup,

	shouldClose,

	-- ** HINT

	hint, B.WindowHint(..),
	B.ClientAPI(..),

	-- * PARAMETER

	getFramebufferSize,

	-- * CALLBACK

	setKeyCallback, KeyCallback,
	setFramebufferSizeCallback, B.FramebufferSizeCallback,

	-- * STATE

	getKey

	) where

import Prelude hiding (lookup)

import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Data.Map qualified as M

import Graphics.UI.GLFW qualified as B
import Graphics.UI.GlfwG.Window.Type

data Group s k = Group TSem (TVar (M.Map k (W s)))

create :: Int -> Int -> String -> Maybe B.Monitor -> Maybe B.Window ->
	(forall s . W s -> IO a) -> IO a
create wd hg ttl mm mws f = group \g -> f . fromRight =<< create' g () wd hg ttl mm mws
	where fromRight = \case Left _ -> error "never occur"; Right w -> w

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

unsafeDestroy :: Ord k => Group s k -> k -> IO (Either String ())
unsafeDestroy (Group sem ws) k = do
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

getFramebufferSize :: W sw -> IO (Int, Int)
getFramebufferSize (W w) = B.getFramebufferSize w

setKeyCallback :: W s -> Maybe (KeyCallback s) -> IO ()
setKeyCallback (W w) = B.setKeyCallback w . ((. W) <$>)

type KeyCallback s = W s -> B.Key -> Int -> B.KeyState -> B.ModifierKeys -> IO ()

shouldClose :: W sw -> IO Bool
shouldClose (W w) = B.windowShouldClose w

getKey :: W sw -> B.Key -> IO B.KeyState
getKey (W w) = B.getKey w
