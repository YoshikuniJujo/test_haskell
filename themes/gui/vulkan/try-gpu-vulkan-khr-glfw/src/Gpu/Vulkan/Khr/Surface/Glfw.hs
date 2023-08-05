{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Surface.Glfw where

import Control.Exception
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry

import qualified Graphics.UI.GLFW as GlfwB

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Instance.Type as Vk.Instance
import qualified Gpu.Vulkan.Khr.Surface.Type as Vk.Khr.Surface
import qualified Gpu.Vulkan.Khr.Surface.Middle as Vk.Khr.Surface.M
import qualified Gpu.Vulkan.Khr.Surface.Glfw.Middle as M

createWindowSurface :: (AllocationCallbacks.ToMiddle mscc ) =>
	Vk.Instance.I si -> GlfwB.Window ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc ->
	(forall ss . Vk.Khr.Surface.S ss -> IO a) -> IO a
createWindowSurface (Vk.Instance.I ist) win
	(AllocationCallbacks.toMiddle -> macc) f = bracket
	(M.createWindowSurface ist win macc)
	(\sfc -> Vk.Khr.Surface.M.destroy ist sfc macc)
	(f . Vk.Khr.Surface.S)
