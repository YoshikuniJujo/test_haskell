{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Glfw where

import Foreign.Storable.PeekPoke
import Control.Exception

import qualified Graphics.UI.GLFW as GlfwB

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Instance.Type as Vk.Instance
import qualified Gpu.Vulkan.Khr.Surface.Type as Vk.Khr.Surface
import qualified Gpu.Vulkan.Khr.Surface.Middle as Vk.Khr.Surface.M
import qualified Glfw.Middle as M

createWindowSurface :: (Pokable c, Pokable d) =>
	Vk.Instance.I si -> GlfwB.Window ->
	Maybe (AllocationCallbacks.A sc c) -> Maybe (AllocationCallbacks.A sd d) ->
	(forall ss . Vk.Khr.Surface.S ss -> IO a) -> IO a
createWindowSurface (Vk.Instance.I ist) win
	((AllocationCallbacks.toMiddle <$>) -> macc)
	((AllocationCallbacks.toMiddle <$>) -> macd) f = bracket
	(M.createWindowSurface ist win macc)
	(\sfc -> Vk.Khr.Surface.M.destroy ist sfc macd)
	(f . Vk.Khr.Surface.S)
