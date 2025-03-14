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
	("ConfigFlagsNone", #{const ImGuiConfigFlags_None}),
	("ConfigFlagsNavEnableKeyboard",
		#{const ImGuiConfigFlags_NavEnableKeyboard}),
	("ConfigFlagsNavEnableGamepad",
		#{const ImGuiConfigFlags_NavEnableGamepad}),
	("ConfigFlagsNoMouse", #{const ImGuiConfigFlags_NoMouse}),
	("ConfigFlagsNoMouseCursorChange",
		#{const ImGuiConfigFlags_NoMouseCursorChange}),
	("ConfigFlagsNoKeyboard", #{const ImGuiConfigFlags_NoKeyboard}),
	("ConfigFlagsIsSrgb", #{const ImGuiConfigFlags_IsSRGB}),
	("ConfigFlagsIsTouchScreen", #{const ImGuiConfigFlags_IsTouchScreen}) ]
