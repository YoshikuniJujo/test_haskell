#include <vulkan/vulkan.h>
#include "imgui.h"
#include "imgui_impl_glfw.h"
#include "imgui_impl_vulkan.h"

extern "C" void im_gui_impl_vulkan_h_create_window_swap_chain(
	VkDevice device,
	ImGui_ImplVulkanH_Window* wd, const VkAllocationCallbacks* allocator,
	int w, int h, uint32_t min_image_count, VkSwapchainKHR );
extern "C" void im_gui_impl_vulkan_h_create_window_command_buffers(
	VkPhysicalDevice physical_device, VkDevice device,
	ImGui_ImplVulkanH_Window* wd, uint32_t queue_family,
	const VkAllocationCallbacks* allocator );
extern "C" void im_gui_impl_vulkan_h_destroy_before_create_swap_chain(
	VkDevice device, ImGui_ImplVulkanH_Window* wd,
	const VkAllocationCallbacks* allocator );
extern "C" void im_gui_impl_vulkan_h_create_swap_chain(
	VkDevice device,
	ImGui_ImplVulkanH_Window* wd,
	uint32_t min_image_count );
extern "C" void im_gui_impl_vulkan_h_only_create_swap_chain(
	VkDevice device,
	ImGui_ImplVulkanH_Window* wd,
	const VkAllocationCallbacks* allocator,
	int w, int h,
	uint32_t min_image_count,
	VkSwapchainKHR old_swapchain,
	VkSurfaceCapabilitiesKHR* cap );
extern "C" VkSwapchainKHR* im_gui_impl_vulkan_h_only_create_swap_chain_no_wd(
	VkDevice device, const VkAllocationCallbacks* allocator,
	uint32_t min_image_count, VkSwapchainKHR old_swapchain,
	VkSurfaceCapabilitiesKHR* cap,
	VkSurfaceKHR sfc,
	VkSurfaceFormatKHR* sfmt, VkPresentModeKHR pm, int wdt, int hgt );
extern "C" void im_gui_impl_vulkan_h_copy_swap_chain_to_wd(
	ImGui_ImplVulkanH_Window* wd, VkSwapchainKHR* pscsrc );
extern "C" void im_gui_impl_vulkan_h_set_size(
	ImGui_ImplVulkanH_Window* wd,
	int w, int h, VkSurfaceCapabilitiesKHR *pcap );

void
im_gui_impl_vulkan_h_create_window_swap_chain(
	VkDevice device,
	ImGui_ImplVulkanH_Window* wd, const VkAllocationCallbacks* allocator,
	int w, int h, uint32_t min_image_count, VkSwapchainKHR old_swapchain)
{
	ImGui_ImplVulkanH_CreateWindowSwapChain(
		device, wd, allocator, w, h, min_image_count, old_swapchain );
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

void
im_gui_impl_vulkan_h_destroy_before_create_swap_chain(
	VkDevice device, ImGui_ImplVulkanH_Window* wd,
	const VkAllocationCallbacks* allocator )
{
	ImGui_ImplVulkanH_DestroyBeforeCreateSwapChain(device, wd, allocator);
}

void
im_gui_impl_vulkan_h_create_swap_chain(
	VkDevice device,
	ImGui_ImplVulkanH_Window* wd,
	uint32_t min_image_count )
{
	ImGui_ImplVulkanH_CreateSwapChain(
		device, wd, min_image_count );
}

void
im_gui_impl_vulkan_h_only_create_swap_chain(
	VkDevice device,
	ImGui_ImplVulkanH_Window* wd,
	const VkAllocationCallbacks* allocator,
	int w, int h,
	uint32_t min_image_count,
	VkSwapchainKHR old_swapchain,
	VkSurfaceCapabilitiesKHR* cap )
{
	ImGui_ImplVulkanH_OnlyCreateSwapChain(
		device, wd, allocator, w, h, min_image_count, old_swapchain, cap );
}

VkSwapchainKHR*
im_gui_impl_vulkan_h_only_create_swap_chain_no_wd(
	VkDevice device,
	const VkAllocationCallbacks* allocator,
	uint32_t min_image_count,
	VkSwapchainKHR old_swapchain,
	VkSurfaceCapabilitiesKHR* cap,

	VkSurfaceKHR sfc,
	VkSurfaceFormatKHR* sfmt,
	VkPresentModeKHR pm,
	int wdt, int hgt )
{
	return ImGui_ImplVulkanH_OnlyCreateSwapChainNoWd(
		device, allocator, min_image_count, old_swapchain, cap,
		sfc, sfmt, pm, wdt, hgt );
}

void
im_gui_impl_vulkan_h_copy_swap_chain_to_wd(
	ImGui_ImplVulkanH_Window* wd, VkSwapchainKHR* pscsrc )
{
	ImGui_ImplVulkanH_CopySwapchainToWd(wd, pscsrc);
}

void
im_gui_impl_vulkan_h_set_size(
	ImGui_ImplVulkanH_Window* wd,
	int w, int h, VkSurfaceCapabilitiesKHR *pcap )
{
	ImGui_ImplVulkanH_SetSize(wd, w, h, pcap);
}
