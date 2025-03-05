#include <cstdlib>
#include "imgui_impl_vulkan.h"
#include "imgui_impl_vulkan_helper_c.h"

void imguiImplVulkanHFramesemaphoresFromC(
	ImGui_ImplVulkanH_FrameSemaphores_C *c,
	struct ImGui_ImplVulkanH_FrameSemaphores *cxx )
{
	cxx->ImageAcquiredSemaphore = c->ImageAcquiredSemaphore;
	cxx->RenderCompleteSemaphore = c->RenderCompleteSemaphore;
}

void imguiImplVulkanHFramesemaphoresToC(
	struct ImGui_ImplVulkanH_FrameSemaphores *cxx,
	ImGui_ImplVulkanH_FrameSemaphores_C *c )
{
	c->ImageAcquiredSemaphore = cxx->ImageAcquiredSemaphore;
	c->RenderCompleteSemaphore = cxx->RenderCompleteSemaphore;
}
