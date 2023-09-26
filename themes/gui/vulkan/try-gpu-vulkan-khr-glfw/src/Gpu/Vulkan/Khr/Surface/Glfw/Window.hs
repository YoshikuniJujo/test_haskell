{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Surface.Glfw.Window (

	-- * CREATE

	create, create'

	) where

import Prelude hiding (lookup)

import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Exception
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry
import Data.Map qualified as Map

import Graphics.UI.GlfwG.Window.Type

import Gpu.Vulkan.AllocationCallbacks.Internal qualified as AllocationCallbacks
import Gpu.Vulkan.Instance.Internal qualified as Vk.Instance
import Gpu.Vulkan.Khr.Surface.Internal qualified as Vk.Khr.Surface
import Gpu.Vulkan.Khr.Surface.Middle qualified as Vk.Khr.Surface.M
import Gpu.Vulkan.Khr.Surface.Glfw.Middle qualified as M

create :: AllocationCallbacks.ToMiddle mac =>
	Vk.Instance.I si -> W sw ->
	TPMaybe.M (U2 AllocationCallbacks.A) mac ->
	(forall ss . Vk.Khr.Surface.S ss -> IO a) -> IO a
create (Vk.Instance.I ist) (W win)
	(AllocationCallbacks.toMiddle -> macc) f = bracket
	(M.createWindowSurface ist win macc)
	(\sfc -> Vk.Khr.Surface.M.destroy ist sfc macc)
	(f . Vk.Khr.Surface.S)

create' :: (Ord k, AllocationCallbacks.ToMiddle ma) =>
	Vk.Instance.I si -> Vk.Khr.Surface.Group ma ss k -> k -> W sw ->
	IO (Either String (Vk.Khr.Surface.S ss))
create' (Vk.Instance.I ist)
	(Vk.Khr.Surface.Group
		(AllocationCallbacks.toMiddle -> ma) sem ss) k (W win) = do
	ok <- atomically do
		mx <- (Map.lookup k) <$> readTVar ss
		case mx of
			Nothing -> waitTSem sem >> pure True
			Just _ -> pure False
	if ok
	then do	s <- M.createWindowSurface ist win ma
		let	s' = Vk.Khr.Surface.S s
		atomically $ modifyTVar ss (Map.insert k s') >> signalTSem sem
		pure $ Right s'
	else pure . Left $
		"Gpu.Vulkan.Khr.Surface.Glfw.Window: The key already exist"
