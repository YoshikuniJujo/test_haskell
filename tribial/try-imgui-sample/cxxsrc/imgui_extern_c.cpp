#include <stdio.h>
#include "imgui.h"
#include "imgui_impl_vulkan.h"
#include "imgui_impl_glfw.h"

struct GLFWwindow;

extern "C" void imgui_check_version();
extern "C" ImGuiContext* create_context_no_arg();
extern "C" ImGuiIO* get_io();
extern "C" ImGuiConfigFlags get_io_config_flags(ImGuiIO&);
extern "C" void set_io_config_flags(ImGuiIO&, ImGuiConfigFlags);
extern "C" void imgui_style_colors_dark_no_arg();
extern "C" void imgui_style_colors_light_no_arg();
extern "C" void imgui_style_colors_classic_no_arg();
extern "C" bool imgui_impl_glfw_init_for_vulkan(GLFWwindow*, bool);
extern "C" bool imgui_impl_vulkan_init(ImGui_ImplVulkan_InitInfo*);
extern "C" ImFontAtlas* imgui_io_fonts(ImGuiIO*);
extern "C" const ImWchar* im_font_atlas_get_glyph_ranges_japanese(ImFontAtlas*);
extern "C" void check_im_wchar();

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

void
imgui_style_colors_dark_no_arg()
{
	ImGui::StyleColorsDark();
}

void
imgui_style_colors_light_no_arg()
{
	ImGui::StyleColorsLight();
}

void
imgui_style_colors_classic_no_arg()
{
	ImGui::StyleColorsClassic();
}

bool
imgui_impl_glfw_init_for_vulkan(GLFWwindow* win, bool ics)
{
	return ImGui_ImplGlfw_InitForVulkan(win, ics);
}

bool
imgui_impl_vulkan_init(ImGui_ImplVulkan_InitInfo* info)
{
	return ImGui_ImplVulkan_Init(info);
}

ImFontAtlas*
imgui_io_fonts(ImGuiIO *pio)
{
	return pio->Fonts;
}

const ImWchar*
im_font_atlas_get_glyph_ranges_japanese(ImFontAtlas *fa)
{
	return fa->GetGlyphRangesJapanese();
}

void check_im_wchar()
{
	printf("check_im_wchar\n");
#ifdef IMGUI_USE_WCHAR32
	printf("IMGUI_USE_WCHAR32 defined\n");
#else
	printf("IMGUI_USE_WCHAR32 not defined\n");
#endif
}
