{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.NoVulkan.Demo.Core where

import Foreign.Ptr
import Data.Word

#include "imgui_c.h"

showWindow :: Ptr #{type bool} -> IO ()
showWindow = cxx_im_gui_show_demo_window

foreign import ccall "im_gui_show_demo_window"
	cxx_im_gui_show_demo_window :: Ptr #{type bool} -> IO ()
