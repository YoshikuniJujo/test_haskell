{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.NoVulkan.Core where

import Foreign.Ptr
import Foreign.C.String
import Data.Word
import Data.Int

#include "imgui_c.h"

newFrame :: IO ()
newFrame = cxx_im_gui_new_frame

foreign import ccall "im_gui_new_frame" cxx_im_gui_new_frame :: IO ()

begin :: CString -> Ptr #{type bool} -> #{type ImGuiWindowFlags} ->
	IO #{type bool}
begin = cxx_im_gui_begin

foreign import ccall "im_gui_begin" cxx_im_gui_begin ::
	CString -> Ptr #{type bool} -> #{type ImGuiWindowFlags} ->
	IO #{type bool}

end :: IO ()
end = cxx_im_gui_end

foreign import ccall "im_gui_end" cxx_im_gui_end :: IO ()

render :: IO ()
render = cxx_im_gui_render

foreign import ccall "im_gui_render" cxx_im_gui_render :: IO ()
