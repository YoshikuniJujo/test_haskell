{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Io.Core (

	get, I(..),

	getConfigFlags, setConfigFlags

	) where

import Foreign.Ptr
import Data.Int

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
