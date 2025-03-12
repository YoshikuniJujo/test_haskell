{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Glfw.Core (init) where

import Prelude hiding (init)
import Foreign.Ptr
import Data.Word

import Graphics.UI.GLFW qualified as GLFW
import Graphics.UI.GLFW.C qualified as GLFW.C
import Bindings.GLFW qualified as BGLFW

#include <stdbool.h>

init :: GLFW.Window -> #{type bool} -> IO #{type bool}
init = cxx_imgui_impl_glfw_init_for_vulkan . GLFW.C.toC

foreign import ccall "imgui_impl_glfw_init_for_vulkan"
	cxx_imgui_impl_glfw_init_for_vulkan ::
	Ptr BGLFW.C'GLFWwindow -> #{type bool} -> IO #{type bool}
