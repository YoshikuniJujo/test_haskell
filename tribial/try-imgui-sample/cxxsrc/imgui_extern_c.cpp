#include "imgui.h"

extern "C" void imgui_check_version();
extern "C" ImGuiContext* create_context_no_arg();
extern "C" ImGuiIO* get_io();
extern "C" ImGuiConfigFlags get_io_config_flags(ImGuiIO&);
extern "C" void set_io_config_flags(ImGuiIO&, ImGuiConfigFlags);

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

ImGuiContext*
create_context_no_arg()
{
	return ImGui::CreateContext();
}

ImGuiIO*
get_io()
{
	return &ImGui::GetIO();
}

ImGuiConfigFlags
get_io_config_flags(ImGuiIO& io)
{
	return io.ConfigFlags;
}

void
set_io_config_flags(ImGuiIO& io, ImGuiConfigFlags fs)
{
	io.ConfigFlags = fs;
}
