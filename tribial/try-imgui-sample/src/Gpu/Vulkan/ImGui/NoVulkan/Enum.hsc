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

enum "WindowFlags" ''#{type ImGuiWindowFlags}
		[''Show, ''Read, ''Eq, ''Bits] [
	("WindowFlagsNone", #{const ImGuiWindowFlags_None_C}),
	("WindowFlagsNoTitleBar", #{const ImGuiWindowFlags_NoTitleBar_C}),
	("WindowFlagsNoResize", #{const ImGuiWindowFlags_NoResize_C}),
	("WindowFlagsNoMove", #{const ImGuiWindowFlags_NoMove_C}),
	("WindowFlagsNoScrollbar", #{const ImGuiWindowFlags_NoScrollbar_C}),
	("WindowFlagsNoScrollWithMouse",
		#{const ImGuiWindowFlags_NoScrollWithMouse_C}),
	("WindowFlagsNoCollapse", #{const ImGuiWindowFlags_NoCollapse_C}),
	("WindowFlagsAlwaysAutoResize",
		#{const ImGuiWindowFlags_AlwaysAutoResize_C}),
	("WindowFlagsNoBackground", #{const ImGuiWindowFlags_NoBackground_C}),
	("WindowFlagsNoSavedSettings",
		#{const ImGuiWindowFlags_NoSavedSettings_C}),
	("WindowFlagsNoMouseInputs", #{const ImGuiWindowFlags_NoMouseInputs_C}),
	("WindowFlagsMenuBar", #{const ImGuiWindowFlags_MenuBar_C}),
	("WindowFlagsHorizontalScrollbar",
		#{const ImGuiWindowFlags_HorizontalScrollbar_C}),
	("WindowFlagsNoFocusOnAppearing",
		#{const ImGuiWindowFlags_NoFocusOnAppearing_C}),
	("WindowFlagsNoBringToFrontOnFocus",
		#{const ImGuiWindowFlags_NoBringToFrontOnFocus_C}),
	("WindowFlagsAlwaysVerticalScrollbar",
		#{const ImGuiWindowFlags_AlwaysVerticalScrollbar_C}),
	("WindowFlagsAlwaysHorizontalScrollbar",
		#{const ImGuiWindowFlags_AlwaysHorizontalScrollbar_C}),
	("WindowFlagsNoNavInputs", #{const ImGuiWindowFlags_NoNavInputs_C}),
	("WindowFlagsNoNavFocus", #{const ImGuiWindowFlags_NoNavFocus_C}),
	("WindowFlagsUnsavedDocument",
		#{const ImGuiWindowFlags_UnsavedDocument_C}),
	("WindowFlagsNoNav", #{const ImGuiWindowFlags_NoNav_C}),
	("WindowFlagsNoDecoration", #{const ImGuiWindowFlags_NoDecoration_C}),
	("WindowFlagsNoInputs", #{const ImGuiWindowFlags_NoInputs_C}),
	("WindowFlagsChildWindow", #{const ImGuiWindowFlags_ChildWindow_C}),
	("WindowFlagsTooltip", #{const ImGuiWindowFlags_Tooltip_C}),
	("WindowFlagsPopup", #{const ImGuiWindowFlags_Popup_C}),
	("WindowFlagsModal", #{const ImGuiWindowFlags_Modal_C}),
	("WindowFlagsChildMenu", #{const ImGuiWindowFlags_ChildMenu_C}),
	("WindowFlagsNavFlattened", #{const ImGuiWindowFlags_NavFlattened_C}),
	("WindowFlagsAlwaysUseWindowPadding",
		#{const ImGuiWindowFlags_AlwaysUseWindowPadding_C}) ]

windowFlagsZero :: WindowFlags
windowFlagsZero = WindowFlags 0
