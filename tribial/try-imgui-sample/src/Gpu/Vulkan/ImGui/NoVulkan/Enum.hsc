{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.NoVulkan.Enum where

import Foreign.C.Enum
import Data.Bits
import Data.Int

#include "imgui_c.h"

enum "ConfigFlags" ''#{type ImGuiConfigFlags} [''Show, ''Read, ''Eq, ''Bits] [
	("ConfigFlagsNone", #{const ImGuiConfigFlags_None_C}),
	("ConfigFlagsNavEnableKeyboard",
		#{const ImGuiConfigFlags_NavEnableKeyboard_C}),
	("ConfigFlagsNavEnableGamepad",
		#{const ImGuiConfigFlags_NavEnableGamepad_C}),
	("ConfigFlagsNoMouse", #{const ImGuiConfigFlags_NoMouse_C}),
	("ConfigFlagsNoMouseCursorChange",
		#{const ImGuiConfigFlags_NoMouseCursorChange_C}),
	("ConfigFlagsNoKeyboard", #{const ImGuiConfigFlags_NoKeyboard_C}),
	("ConfigFlagsIsSrgb", #{const ImGuiConfigFlags_IsSRGB_C}),
	("ConfigFlagsIsTouchScreen", #{const ImGuiConfigFlags_IsTouchScreen_C}) ]

enum "ImGuiWindowFlags" ''#{type ImGuiWindowFlags}
		[''Show, ''Read, ''Eq, ''Bits] [
	("ImGuiWindowFlagsNone", #{const ImGuiWindowFlags_None_C}),
	("ImGuiWindowFlagsNoTitleBar", #{const ImGuiWindowFlags_NoTitleBar_C}),
	("ImGuiwindowFlagsNoResize", #{const ImGuiWindowFlags_NoResize_C})
	]
