#include <cstdlib>
#include "imgui_impl_vulkan.h"
#include "imgui_impl_vulkan_helper_c.h"

extern "C" void imguiImplVulkanHWindowFromC(
	ImGui_ImplVulkanH_Window_C*, struct ImGui_ImplVulkanH_Window* );

extern "C" void imguiImplVulkanHWindowToC(
	struct ImGui_ImplVulkanH_Window*, ImGui_ImplVulkanH_Window_C* );

extern "C" size_t sizeofImguiImplVulkanHWindow();
extern "C" size_t alignofImguiImplVulkanHWindow();

extern "C" ImGui_ImplVulkanH_Window_C*
	copyImguiImplVulkanHWindowC(ImGui_ImplVulkanH_Window_C*);

extern "C" void freeImguiImplVulkanHWindowC(ImGui_ImplVulkanH_Window_C*);

void imguiImplVulkanHWindowFromC(
	ImGui_ImplVulkanH_Window_C *c, struct ImGui_ImplVulkanH_Window *cxx )
{
	cxx->Width = c->Width; cxx->Height = c->Height;
	cxx->Swapchain = c->Swapchain;
	cxx->Surface = c->Surface; cxx->SurfaceFormat = c->SurfaceFormat;
	cxx->PresentMode = c->PresentMode;
	cxx->RenderPass = c->RenderPass;
	cxx->Pipeline = c->Pipeline;
	cxx->UseDynamicRendering = c->UseDynamicRendering;
	cxx->ClearEnable = c->ClearEnable; cxx->ClearValue = c->ClearValue;
	cxx->FrameIndex = c->FrameIndex; cxx->ImageCount = c->ImageCount;
	cxx->SemaphoreCount = c->SemaphoreCount;
	cxx->SemaphoreIndex = c->SemaphoreIndex;
	cxx->Frames.Size = c->Framec;
	cxx->Frames.Capacity = c->Framec;
	cxx->Frames.Data = c->pFrames;
	cxx->FrameSemaphores.Size = c->FrameSemaphorec;
	cxx->FrameSemaphores.Capacity = c->FrameSemaphorec;
	cxx->FrameSemaphores.Data = c->pFrameSemaphores;
}

void imguiImplVulkanHWindowToC(
	struct ImGui_ImplVulkanH_Window *cxx, ImGui_ImplVulkanH_Window_C *c )
{
	c->Width = cxx->Width; c->Height = cxx->Height;
	c->Swapchain = cxx->Swapchain;
	c->Surface = cxx->Surface; c->SurfaceFormat = cxx->SurfaceFormat;
	c->PresentMode = cxx->PresentMode;
	c->RenderPass = cxx->RenderPass;
	c->Pipeline = cxx->Pipeline;
	c->UseDynamicRendering = cxx->UseDynamicRendering;
	c->ClearEnable = cxx->ClearEnable;
	c->FrameIndex = cxx->FrameIndex; c->ImageCount = cxx->ImageCount;
	c->SemaphoreCount = cxx->SemaphoreCount;
	c->SemaphoreIndex = cxx->SemaphoreIndex;
	c->Framec = cxx->Frames.Size;
	c->pFrames = cxx->Frames.Data;
	c->FrameSemaphorec = cxx->FrameSemaphores.Size;
	c->pFrameSemaphores = cxx->FrameSemaphores.Data;
}

size_t sizeofImguiImplVulkanHWindow()
{
	return sizeof(struct ImGui_ImplVulkanH_Window);
}

size_t alignofImguiImplVulkanHWindow()
{
	return alignof(struct ImGui_ImplVulkanH_Window);
}

ImGui_ImplVulkanH_Window_C*
copyImguiImplVulkanHWindowC(ImGui_ImplVulkanH_Window_C* c)
{
	ImGui_ImplVulkanH_Window_C *p =
		(ImGui_ImplVulkanH_Window_C *)malloc(
			sizeof(ImGui_ImplVulkanH_Window_C));
	return p;
}

void
freeImguiImplVulkanHWindowC(ImGui_ImplVulkanH_Window_C* c)
{
	free(c);
}
