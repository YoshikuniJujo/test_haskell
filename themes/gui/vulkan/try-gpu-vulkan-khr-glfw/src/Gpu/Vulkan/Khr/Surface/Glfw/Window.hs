{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Surface.Glfw.Window (

	-- * CREATE

	create

	) where

import Prelude hiding (lookup)

import Control.Exception
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry

import Graphics.UI.GlfwG.Window.Type

import Gpu.Vulkan.AllocationCallbacks.Internal qualified as AllocationCallbacks
import Gpu.Vulkan.Instance.Internal qualified as Vk.Instance
import Gpu.Vulkan.Khr.Surface.Type qualified as Vk.Khr.Surface
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
