#include "imgui.h"

extern "C" void imgui_check_version();

void
imgui_check_version()
{
	ImGui::DebugCheckVersionAndDataLayout(
		IMGUI_VERSION,
		sizeof(ImGuiIO), sizeof(ImGuiStyle), sizeof(ImVec2),
		sizeof(ImVec4), sizeof(ImDrawVert), sizeof(ImDrawIdx) );
}

// Check that version and structures layouts are matching between compiled imgui code and caller. Read comments above DebugCheckVersionAndDataLayout() for details.
#define IMGUI_CHECKVERSION()        ImGui::DebugCheckVersionAndDataLayout(IMGUI_VERSION, sizeof(ImGuiIO), sizeof(ImGuiStyle), sizeof(ImVec2), sizeof(ImVec4), sizeof(ImDrawVert), sizeof(ImDrawIdx))
