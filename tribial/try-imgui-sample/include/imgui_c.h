#pragma once

typedef int ImGuiConfigFlags;

enum ImGuiConfigFlags_
{
    ImGuiConfigFlags_None                   = 0,
    ImGuiConfigFlags_NavEnableKeyboard      = 1 << 0,   // Master keyboard navigation enable flag. Enable full Tabbing + directional arrows + space/enter to activate.
    ImGuiConfigFlags_NavEnableGamepad       = 1 << 1,   // Master gamepad navigation enable flag. Backend also needs to set ImGuiBackendFlags_HasGamepad.
    ImGuiConfigFlags_NoMouse                = 1 << 4,   // Instruct dear imgui to disable mouse inputs and interactions.
    ImGuiConfigFlags_NoMouseCursorChange    = 1 << 5,   // Instruct backend to not alter mouse cursor shape and visibility. Use if the backend cursor changes are interfering with yours and you don't want to use SetMouseCursor() to change mouse cursor. You may want to honor requests from imgui by reading GetMouseCursor() yourself instead.
    ImGuiConfigFlags_NoKeyboard             = 1 << 6,   // Instruct dear imgui to disable keyboard inputs and interactions. This is done by ignoring keyboard events and clearing existing states.

    // User storage (to allow your backend/engine to communicate to code that may be shared between multiple projects. Those flags are NOT used by core Dear ImGui)
    ImGuiConfigFlags_IsSRGB                 = 1 << 20,  // Application is SRGB-aware.
    ImGuiConfigFlags_IsTouchScreen          = 1 << 21,  // Application is using a touch screen instead of a mouse.

#ifndef IMGUI_DISABLE_OBSOLETE_FUNCTIONS
    ImGuiConfigFlags_NavEnableSetMousePos   = 1 << 2,   // [moved/renamed in 1.91.4] -> use bool io.ConfigNavMoveSetMousePos
    ImGuiConfigFlags_NavNoCaptureKeyboard   = 1 << 3,   // [moved/renamed in 1.91.4] -> use bool io.ConfigNavCaptureKeyboard
#endif
};
