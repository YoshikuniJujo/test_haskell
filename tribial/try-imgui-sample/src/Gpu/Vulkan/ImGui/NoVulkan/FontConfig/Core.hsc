{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.NoVulkan.FontConfig.Core where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.Struct
import Data.Word
import Data.Int
import Data.ByteString qualified as BS

import Gpu.Vulkan.ImGui.NoVulkan.Font.Core qualified as Font

#include "imgui_c.h"

data FTag
type F = Ptr FTag

type PtrCChar = Ptr CChar
type PtrUShort = Ptr #{type unsigned short}

struct "FC" #{size ImFontConfig_C} #{alignment ImFontConfig_C} [
	("FontData", ''PtrCChar,
		[| #{peek ImFontConfig_C, FontData} |],
		[| #{poke ImFontConfig_C, FontData} |]),
	("FontDataSize", ''#{type int},
		[| #{peek ImFontConfig_C, FontDataSize} |],
		[| #{poke ImFontConfig_C, FontDataSize} |]),
	("FontDataOwnedByAtlas", ''#{type bool},
		[| #{peek ImFontConfig_C, FontDataOwnedByAtlas} |],
		[| #{poke ImFontConfig_C, FontDataOwnedByAtlas} |]),
	("MergeMode", ''#{type bool},
		[| #{peek ImFontConfig_C, MergeMode} |],
		[| #{poke ImFontConfig_C, MergeMode} |]),
	("PixelSnapH", ''#{type bool},
		[| #{peek ImFontConfig_C, PixelSnapH} |],
		[| #{poke ImFontConfig_C, PixelSnapH} |]),
	("FontNo", ''#{type int},
		[| #{peek ImFontConfig_C, FontNo} |],
		[| #{poke ImFontConfig_C, FontNo} |]),
	("OversampleH", ''#{type int},
		[| #{peek ImFontConfig_C, OversampleH} |],
		[| #{poke ImFontConfig_C, OversampleH} |]),
	("OversampleV", ''#{type int},
		[| #{peek ImFontConfig_C, OversampleV} |],
		[| #{poke ImFontConfig_C, OversampleV} |]),
	("SizePixels", ''#{type float},
		[| #{peek ImFontConfig_C, SizePixels} |],
		[| #{poke ImFontConfig_C, SizePixels} |]),
	("GlyphOffsetX", ''#{type float},
		[| #{peek ImFontConfig_C, GlyphOffsetX} |],
		[| #{poke ImFontConfig_C, GlyphOffsetX} |]),
	("GlyphOffsetY", ''#{type float},
		[| #{peek ImFontConfig_C, GlyphOffsetY} |],
		[| #{poke ImFontConfig_C, GlyphOffsetY} |]),
	("GlyphRanges", ''PtrUShort,
		[| #{peek ImFontConfig_C, GlyphRanges} |],
		[| #{poke ImFontConfig_C, GlyphRanges} |]),
	("GlyphMinAdvanceX", ''#{type float},
		[| #{peek ImFontConfig_C, GlyphMinAdvanceX} |],
		[| #{poke ImFontConfig_C, GlyphMinAdvanceX} |]),
	("GlyphMaxAdvanceX", ''#{type float},
		[| #{peek ImFontConfig_C, GlyphMaxAdvanceX} |],
		[| #{poke ImFontConfig_C, GlyphMaxAdvanceX} |]),
	("GlyphExtraAdvanceX", ''#{type float},
		[| #{peek ImFontConfig_C, GlyphExtraAdvanceX} |],
		[| #{poke ImFontConfig_C, GlyphExtraAdvanceX} |]),
	("FontBuilderFlags", ''#{type unsigned int},
		[| #{peek ImFontConfig_C, FontBuilderFlags} |],
		[| #{poke ImFontConfig_C, FontBuilderFlags} |]),
	("RasterizerMultiply", ''#{type float},
		[| #{peek ImFontConfig_C, RasterizerMultiply} |],
		[| #{poke ImFontConfig_C, RasterizerMultiply} |]),
	("RasterizerDensity", ''#{type float},
		[| #{peek ImFontConfig_C, RasterizerDensity} |],
		[| #{poke ImFontConfig_C, RasterizerDensity} |]),
	("EllipsisChar", ''#{type unsigned short},
		[| #{peek ImFontConfig_C, EllipsisChar} |],
		[| #{poke ImFontConfig_C, EllipsisChar} |]),
	("Name", ''BS.ByteString,
		[| \p -> peekByteString (#{ptr ImFontConfig_C, Name} p) 40 |],
		[| \p bs ->
			pokeByteString (#{ptr ImFontConfig_C, Name} p) 40 bs |]),
	("DstFont", ''Font.F,
		[| #{peek ImFontConfig_C, DstFont} |],
		[| #{poke ImFontConfig_C, DstFont} |]) ]
	[''Show, ''Storable]

toC :: F -> IO FC
toC f = alloca \pfc -> cxx_im_font_config_to_c f pfc >> peek pfc

foreign import ccall "im_font_config_to_c" cxx_im_font_config_to_c ::
	F -> Ptr FC -> IO ()

peekByteString :: Ptr CChar -> Int -> IO BS.ByteString
peekByteString = curry BS.packCStringLen

pokeByteString :: Ptr CChar -> Int -> BS.ByteString -> IO ()
pokeByteString p n bs =
	BS.useAsCStringLen bs \(p', n') -> copyBytes p p' (min n n')
