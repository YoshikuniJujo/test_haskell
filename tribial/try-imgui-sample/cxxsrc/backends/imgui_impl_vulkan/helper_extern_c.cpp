#include <vulkan/vulkan.h>
#include "imgui.h"
#include "imgui_impl_glfw.h"
#include "imgui_impl_vulkan.h"

extern "C" void im_gui_impl_vulkan_h_create_window_swap_chain(
	VkPhysicalDevice physical_device, VkDevice device,
	ImGui_ImplVulkanH_Window* wd, const VkAllocationCallbacks* allocator,
	int w, int h, uint32_t min_image_count);
extern "C" void im_gui_impl_vulkan_h_create_window_command_buffers(
	VkPhysicalDevice physical_device, VkDevice device,
	ImGui_ImplVulkanH_Window* wd, uint32_t queue_family,
	const VkAllocationCallbacks* allocator);

void
im_gui_impl_vulkan_h_create_window_swap_chain(
	VkPhysicalDevice physical_device, VkDevice device,
	ImGui_ImplVulkanH_Window* wd, const VkAllocationCallbacks* allocator,
	int w, int h, uint32_t min_image_count)
{
	ImGui_ImplVulkanH_CreateWindowSwapChain(
		physical_device, device, wd, allocator, w, h, min_image_count );
}

void
im_gui_impl_vulkan_h_create_window_command_buffers(
	VkPhysicalDevice physical_device, VkDevice device,
	ImGui_ImplVulkanH_Window* wd, uint32_t queue_family,
	const VkAllocationCallbacks* allocator)
{
	ImGui_ImplVulkanH_CreateWindowCommandBuffers(
		physical_device, device, wd, queue_family, allocator );
}
