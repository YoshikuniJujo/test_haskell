{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.NoVulkan.Core where

newFrame :: IO ()
newFrame = cxx_im_gui_new_frame

foreign import ccall "im_gui_new_frame" cxx_im_gui_new_frame :: IO ()
