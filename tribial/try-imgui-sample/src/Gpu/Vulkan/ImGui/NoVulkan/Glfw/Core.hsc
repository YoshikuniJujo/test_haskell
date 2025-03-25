module Gpu.Vulkan.ImGui.NoVulkan.Glfw.Core where

newFrame :: IO ()
newFrame = cxx_im_gui_impl_glfw_new_frame

foreign import ccall "im_gui_impl_glfw_new_frame"
	cxx_im_gui_impl_glfw_new_frame :: IO ()
