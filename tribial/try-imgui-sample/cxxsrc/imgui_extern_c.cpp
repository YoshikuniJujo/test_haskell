#include <stdio.h>
#include "imgui.h"
#include "imgui_impl_vulkan.h"
#include "imgui_impl_glfw.h"
#include "imgui_c.h"

struct GLFWwindow;
struct ImDrawData;

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
extern "C" ImFontConfig* im_font_atlas_sources(ImFontAtlas *fa, int *sz);
extern "C" int im_font_config_c_size();
extern "C" int im_font_config_size();
extern "C" void im_font_config_to_c(ImFontConfig *cxx, struct ImFontConfig_C *c);
extern "C" void im_font_atlas_clear_fonts(ImFontAtlas*);
extern "C" ImFont* im_font_atlas_add_font(ImFontAtlas* fa, const ImFontConfig* font_cfg);
extern "C" ImFontConfig* im_font_config_new();
extern "C" ImFont* im_font_atlas_add_font_from_file_ttf(
	ImFontAtlas*, const char*, float, const ImFontConfig*, const ImWchar* );
extern "C" void im_gui_impl_vulkan_new_frame();
extern "C" void im_gui_impl_glfw_new_frame();
extern "C" void im_gui_new_frame();
extern "C" void im_gui_show_demo_window(bool*);
extern "C" void im_gui_begin(const char* name, bool* p_open, ImGuiWindowFlags flags);
extern "C" void im_gui_end();
extern "C" void im_gui_render();
extern "C" ImDrawData* im_gui_get_draw_data();
extern "C" float im_draw_data_display_size_x(ImDrawData* dd);
extern "C" float im_draw_data_display_size_y(ImDrawData* dd);

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

ImFontConfig*
im_font_atlas_sources(ImFontAtlas *fa, int *sz)
{
	ImVector<ImFontConfig> vfc = fa->Sources;
	*sz = vfc.Size;
	return vfc.Data;
}

int
im_font_config_c_size()
{
	return sizeof(struct ImFontConfig_C);
}

int
im_font_config_size()
{
	return sizeof(struct ImFontConfig);
}

void
im_font_config_to_c(ImFontConfig *cxx, struct ImFontConfig_C *c)
{
	c->FontData = cxx->FontData;
	c->FontDataSize = cxx->FontDataSize;
	c->FontDataOwnedByAtlas = cxx->FontDataOwnedByAtlas;
	c->MergeMode = cxx->MergeMode;
	c->PixelSnapH = cxx->PixelSnapH;
	c->FontNo = cxx->FontNo;
	c->OversampleH = cxx->OversampleH;
	c->OversampleV = cxx->OversampleV;
	c->SizePixels = cxx->SizePixels;
	c->GlyphOffsetX = cxx->GlyphOffset.x;
	c->GlyphOffsetY = cxx->GlyphOffset.y;
	c->GlyphRanges = cxx->GlyphRanges;
	c->GlyphMinAdvanceX = cxx->GlyphMinAdvanceX;
	c->GlyphMaxAdvanceX = cxx->GlyphMaxAdvanceX;
	c->GlyphExtraAdvanceX = cxx->GlyphExtraAdvanceX;
	c->FontBuilderFlags = cxx->FontBuilderFlags;
	c->RasterizerMultiply = cxx->RasterizerMultiply;
	c->RasterizerDensity = cxx->RasterizerDensity;
	c->EllipsisChar = cxx->EllipsisChar;
	strncpy(c->Name, cxx->Name, 40);
	c->DstFont = cxx->DstFont;

	printf("%d\n", c->FontDataSize);
	printf("%d\n", c->FontDataOwnedByAtlas);
	printf("%d\n", c->MergeMode);
	printf("%lf\n", c->SizePixels);
	printf("c->GlyphOffsetX = %lf\n", c->GlyphOffsetX);
	printf("c->GlyphOffsetY = %lf\n", c->GlyphOffsetY);
	printf("%s\n", c->Name);
}

void
im_font_atlas_clear_fonts(ImFontAtlas* fa)
{
	fa->ClearFonts();
}

ImFont*
im_font_atlas_add_font(ImFontAtlas* fa, const ImFontConfig* font_cfg)
{
	return fa->AddFont(font_cfg);
}

ImFontConfig*
im_font_config_new()
{
	return new ImFontConfig();
}

ImFont*
im_font_atlas_add_font_from_file_ttf(
	ImFontAtlas* fa,
	const char* filename, float size_pixels,
	const ImFontConfig* font_cfg, const ImWchar* glyph_ranges )
{
	return fa->AddFontFromFileTTF(filename, size_pixels, font_cfg, glyph_ranges);
}

void
im_gui_impl_vulkan_new_frame()
{
	ImGui_ImplVulkan_NewFrame();
}

void
im_gui_impl_glfw_new_frame()
{
	ImGui_ImplGlfw_NewFrame();
}

void
im_gui_new_frame()
{
	ImGui::NewFrame();
}

void
im_gui_show_demo_window(bool *p_open)
{
	ImGui::ShowDemoWindow(p_open);
}

void
im_gui_begin(const char* name, bool* p_open, ImGuiWindowFlags flags)
{
	ImGui::Begin(name, p_open, flags);
}

void
im_gui_end()
{
	ImGui::End();
}

void
im_gui_render()
{
	ImGui::Render();
}

ImDrawData*
im_gui_get_draw_data()
{
	return ImGui::GetDrawData();
}

float
im_draw_data_display_size_x(ImDrawData* dd)
{
	return dd->DisplaySize.x;
}

float
im_draw_data_display_size_y(ImDrawData* dd)
{
	return dd->DisplaySize.y;
}
