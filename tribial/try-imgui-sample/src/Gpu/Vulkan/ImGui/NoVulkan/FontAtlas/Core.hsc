{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.NoVulkan.FontAtlas.Core where

import Foreign.Ptr
import Data.Word

#include "imgui_c.h"

#ifdef IMGUI_USE_WCHAR32
type ImWChar = #{type ImWchar32}
#else
type ImWChar = #{type ImWChar16}
#endif

type F = Ptr FTag
data FTag

getGlyphRangesJapanese :: F -> Ptr ImWChar
getGlyphRangesJapanese = cxx_im_font_atlas_get_glyph_ranges_japanese

foreign import ccall "im_font_atlas_get_glyph_ranges_japanese"
	cxx_im_font_atlas_get_glyph_ranges_japanese :: F -> Ptr ImWChar
