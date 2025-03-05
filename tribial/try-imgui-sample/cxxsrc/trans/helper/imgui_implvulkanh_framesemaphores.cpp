#include <cstdlib>
#include "imgui_impl_vulkan.h"
#include "imgui_impl_vulkan_helper_c.h"

extern "C" void imguiImplVulkanHFramesemaphoresFromC(
	ImGui_ImplVulkanH_FrameSemaphores_C*,
	struct ImGui_ImplVulkanH_FrameSemaphores* );

extern "C" void imguiImplVulkanHFramesemaphoresToC(
	struct ImGui_ImplVulkanH_FrameSemaphores*,
	ImGui_ImplVulkanH_FrameSemaphores_C* );

extern "C" size_t sizeofImguiImplVulkanHFramesemaphroes();
extern "C" size_t alignofImguiImplVulkanHFramesemaphroes();

extern "C" ImGui_ImplVulkanH_FrameSemaphores_C*
	copyImguiImplVulkanHFrameSemaphoresC(
		ImGui_ImplVulkanH_FrameSemaphores_C* );

extern "C" void freeImguiImplVulkanHFrameSemaphoresC(
	ImGui_ImplVulkanH_FrameSemaphores_C* );

void
imguiImplVulkanHFramesemaphoresFromC(
	ImGui_ImplVulkanH_FrameSemaphores_C *c,
	struct ImGui_ImplVulkanH_FrameSemaphores *cxx )
{
	cxx->ImageAcquiredSemaphore = c->ImageAcquiredSemaphore;
	cxx->RenderCompleteSemaphore = c->RenderCompleteSemaphore;
}

void
imguiImplVulkanHFramesemaphoresToC(
	struct ImGui_ImplVulkanH_FrameSemaphores *cxx,
	ImGui_ImplVulkanH_FrameSemaphores_C *c )
{
	c->ImageAcquiredSemaphore = cxx->ImageAcquiredSemaphore;
	c->RenderCompleteSemaphore = cxx->RenderCompleteSemaphore;
}

size_t
sizeofImguiImplVulkanHFramesemaphores()
{
	return sizeof(struct ImGui_ImplVulkanH_FrameSemaphores);
}

size_t
alignofImguiImplVulkanHFramesemaphores()
{
	return alignof(struct ImGui_ImplVulkanH_FrameSemaphores);
}

ImGui_ImplVulkanH_FrameSemaphores_C*
copyImguiImplVulkahHFramesemaphoresC(ImGui_ImplVulkanH_FrameSemaphores_C* c)
{
	ImGui_ImplVulkanH_FrameSemaphores_C *p =
		(ImGui_ImplVulkanH_FrameSemaphores_C *)malloc(
			sizeof(ImGui_ImplVulkanH_FrameSemaphores_C));
	memcpy(p, c, sizeof(ImGui_ImplVulkanH_FrameSemaphores_C));
	return p;
}

void
freeImguiImplVulkanHFramesemaphoresC(ImGui_ImplVulkanH_FrameSemaphores_C* c)
{
	free(c);
}
