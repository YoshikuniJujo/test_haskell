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

data DrawDataTag
type DrawData = Ptr DrawDataTag

getDrawData :: IO DrawData
getDrawData = cxx_im_gui_get_draw_data

foreign import ccall "im_gui_get_draw_data"
	cxx_im_gui_get_draw_data :: IO DrawData

drawDataDisplaySize :: DrawData -> (#{type float}, #{type float})
drawDataDisplaySize dd = (
	cxx_im_draw_data_display_size_x dd,
	cxx_im_draw_data_display_size_y dd )

foreign import ccall "im_draw_data_display_size_x"
	cxx_im_draw_data_display_size_x :: DrawData -> #{type float}

foreign import ccall "im_draw_data_display_size_y"
	cxx_im_draw_data_display_size_y :: DrawData -> #{type float}
