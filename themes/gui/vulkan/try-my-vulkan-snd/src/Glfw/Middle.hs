{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Glfw.Middle where

import Foreign.Marshal
import Foreign.Storable
import Data.TypeLevel.ParMaybe qualified as TPMaybe

import Gpu.Vulkan.Exception.Middle

import qualified Graphics.UI.GLFW as GlfwB
import Gpu.Vulkan.AllocationCallbacks.Middle.Internal
	qualified as AllocationCallbacks
import qualified Gpu.Vulkan.Instance.Middle.Internal as Instance
import qualified Gpu.Vulkan.Khr.Surface.Middle.Internal as Surface

createWindowSurface :: Instance.I ->
	GlfwB.Window -> TPMaybe.M AllocationCallbacks.A mn -> IO Surface.S
createWindowSurface (Instance.I ist) win mac = Surface.S <$>
	alloca \psfc -> do
		AllocationCallbacks.mToCore mac \pac -> do
			r <- GlfwB.createWindowSurface ist win pac psfc
			throwUnlessSuccess r
		peek psfc
