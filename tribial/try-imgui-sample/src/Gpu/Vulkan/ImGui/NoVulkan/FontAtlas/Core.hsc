{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.NoVulkan.FontAtlas.Core where

import Foreign.Ptr
import Foreign.C.String
import Data.Word
import Data.Int

import Gpu.Vulkan.ImGui.NoVulkan.Font.Core qualified as Font
import Gpu.Vulkan.ImGui.NoVulkan.FontConfig.Core qualified as FontConfig
import Gpu.Vulkan.ImGui.NoVulkan.FontConfigNew.Core qualified as FontConfigNew

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

addFontFromFileTtf ::
	F -> CString -> Float -> FontConfigNew.F -> Ptr ImWChar -> IO Font.F
addFontFromFileTtf = cxx_im_font_atlas_add_font_from_file_ttf

foreign import ccall "im_font_atlas_get_glyph_ranges_japanese"
	cxx_im_font_atlas_get_glyph_ranges_japanese :: F -> Ptr ImWChar

foreign import ccall "im_font_atlas_sources"
	cxx_im_font_atlas_sources :: F -> Ptr #{type int} -> IO FontConfig.F

foreign import ccall "im_font_atlas_clear_fonts"
	cxx_im_font_atlas_clear_fonts :: F -> IO ()

foreign import ccall "im_font_atlas_add_font"
	cxx_im_font_atlas_add_font :: F -> FontConfig.F -> IO ()

foreign import ccall "im_font_atlas_add_font_from_file_ttf"
	cxx_im_font_atlas_add_font_from_file_ttf ::
	F -> CString -> #{type float} -> FontConfigNew.F -> Ptr ImWChar -> IO Font.F
