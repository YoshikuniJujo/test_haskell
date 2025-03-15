{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.NoVulkan.Io.Core (

	get, I(..),

	getConfigFlags, setConfigFlags,

	fonts

	) where

import Foreign.Ptr
import Data.Int

import Gpu.Vulkan.ImGui.NoVulkan.FontAtlas.Core qualified as FontAtlas

#include "imgui_c.h"

get :: IO I
get = cxx_get_io

newtype I = I (Ptr ITag)
data ITag

foreign import ccall "get_io" cxx_get_io :: IO I

getConfigFlags :: I -> IO #{type ImGuiConfigFlags}
getConfigFlags = cxx_get_io_config_flags

foreign import ccall "get_io_config_flags" cxx_get_io_config_flags ::
	I -> IO #{type ImGuiConfigFlags}

setConfigFlags :: I -> #{type ImGuiConfigFlags} -> IO ()
setConfigFlags = cxx_set_io_config_flags

foreign import ccall "set_io_config_flags" cxx_set_io_config_flags ::
	I -> #{type ImGuiConfigFlags} -> IO ()

fonts :: I -> FontAtlas.F
fonts = cxx_imgui_io_fonts

foreign import ccall "imgui_io_fonts" cxx_imgui_io_fonts :: I -> FontAtlas.F
