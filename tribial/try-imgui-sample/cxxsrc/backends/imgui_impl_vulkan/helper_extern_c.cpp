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
	const VkAllocationCallbacks* allocator, uint32_t ic );
extern "C" VkCommandPool *
	im_gui_impl_vulkan_h_create_window_command_buffers_create_command_pool(
	VkDevice device, uint32_t queue_family,
	const VkAllocationCallbacks* allocator, uint32_t ic );
extern "C" void
	im_gui_impl_vulkan_h_create_window_command_buffers_from_command_pool(
	VkDevice device,
	ImGui_ImplVulkanH_Window* wd, uint32_t queue_family,
	const VkAllocationCallbacks* allocator,
	VkCommandPool *cps );
extern "C" void
	im_gui_impl_vulkan_h_create_window_command_buffers_from_command_pool2(
	VkDevice device,
	ImGui_ImplVulkanH_Window* wd, uint32_t queue_family,
	const VkAllocationCallbacks* allocator );
extern "C" void
im_gui_impl_vulkan_h_create_window_command_buffers_frames(
	VkDevice device,
	ImGui_ImplVulkanH_Window* wd, uint32_t queue_family,
	const VkAllocationCallbacks* allocator );
extern "C" void
im_gui_impl_vulkan_h_create_window_command_buffers_semaphores(
	VkDevice device,
	ImGui_ImplVulkanH_Window* wd,
	const VkAllocationCallbacks* allocator );
extern "C" void
	im_gui_impl_vulkan_h_create_window_command_buffers_copy_command_pool(
	ImGui_ImplVulkanH_Window* wd, VkCommandPool *cps );
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
extern "C" void im_gui_impl_vulkan_h_create_swap_chain_modify_wd(
	ImGui_ImplVulkanH_Window* wd, VkImage* backbuffers, int i );
extern "C" void im_gui_impl_vulkan_h_create_window_render_pass(
	VkDevice device, ImGui_ImplVulkanH_Window* wd,
	const VkAllocationCallbacks* allocator );
extern "C" void im_gui_impl_vulkan_h_create_window_image_views(
	VkDevice device, ImGui_ImplVulkanH_Window* wd,
	const VkAllocationCallbacks* allocator );
extern "C" void im_gui_impl_vulkan_h_create_window_framebuffer(
	VkDevice device, ImGui_ImplVulkanH_Window* wd,
	const VkAllocationCallbacks* allocator );
extern "C" VkRenderPass* im_gui_impl_vulkan_h_create_window_render_pass_raw(
	VkDevice device, const VkAllocationCallbacks* allocator,
	bool udr, VkFormat fmt, bool ce );
extern "C" void im_gui_impl_vulkan_h_set_wd_render_pass(
	ImGui_ImplVulkanH_Window* wd, VkRenderPass *rp );
extern "C" VkImageView* im_gui_impl_vulkan_h_create_window_image_views_raw(
	VkDevice device, VkFormat fmt, uint32_t im_count, VkImage* imgs,
	const VkAllocationCallbacks* allocator );
extern "C" void im_gui_impl_vulkan_h_copy_image_views_to_wd(
	ImGui_ImplVulkanH_Window* wd, VkImageView* views );
extern "C" VkFramebuffer* im_gui_impl_vulkan_h_create_window_framebuffer_raw(
	VkDevice device, const VkAllocationCallbacks* allocator,
	bool udr, int im_count, VkRenderPass *rp, int wdt, int hgt, VkImageView* bv );
extern "C" void im_gui_impl_vulkan_h_copy_framebuffer_to_wd(
	bool udr, ImGui_ImplVulkanH_Window* wd,
	int im_count, VkFramebuffer* fbs );

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
	const VkAllocationCallbacks* allocator, uint32_t ic )
{
	ImGui_ImplVulkanH_CreateWindowCommandBuffers(
		physical_device, device, wd, queue_family, allocator, ic );
}

VkCommandPool *
im_gui_impl_vulkan_h_create_window_command_buffers_create_command_pool(
	VkDevice device, uint32_t queue_family,
	const VkAllocationCallbacks* allocator, uint32_t ic )
{
	return ImGui_ImplVulkanH_CreateWindowCommandBuffersCreateCommandPool(
		device, queue_family, allocator, ic );
}

void
im_gui_impl_vulkan_h_create_window_command_buffers_from_command_pool(
	VkDevice device,
	ImGui_ImplVulkanH_Window* wd, uint32_t queue_family,
	const VkAllocationCallbacks* allocator,
	VkCommandPool *cps )
{
	ImGui_ImplVulkanH_CreateWindowCommandBuffersFromCommandPool(
		device, wd, queue_family, allocator, cps );
}

void
im_gui_impl_vulkan_h_create_window_command_buffers_from_command_pool2(
	VkDevice device,
	ImGui_ImplVulkanH_Window* wd, uint32_t queue_family,
	const VkAllocationCallbacks* allocator )
{
	ImGui_ImplVulkanH_CreateWindowCommandBuffersFromCommandPool2(
		device, wd, queue_family, allocator );
}

void
im_gui_impl_vulkan_h_create_window_command_buffers_frames(
	VkDevice device,
	ImGui_ImplVulkanH_Window* wd, uint32_t queue_family,
	const VkAllocationCallbacks* allocator )
{
	ImGui_ImplVulkanH_CreateWindowCommandBuffersFrames(
		device, wd, queue_family, allocator );
}

void
im_gui_impl_vulkan_h_create_window_command_buffers_frames_command_buffers2(
	VkDevice device, ImGui_ImplVulkanH_Window* wd )
{
	ImGui_ImplVulkanH_CreateWindowCommandBuffersFramesCommandBuffers2(
		device, wd );
}

void
im_gui_impl_vulkan_h_create_window_command_buffers_semaphores(
	VkDevice device,
	ImGui_ImplVulkanH_Window* wd,
	const VkAllocationCallbacks* allocator )
{
	ImGui_ImplVulkanH_CreateWindowCommandBuffersSemaphores(
		device, wd, allocator );
}

void
im_gui_impl_vulkan_h_create_window_command_buffers_copy_command_pool(
	ImGui_ImplVulkanH_Window* wd, VkCommandPool *cps )
{
	ImGui_ImplVulkanH_CreateWindowCommandBuffersCopyCommandPool(wd, cps);
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

void
im_gui_impl_vulkan_h_create_swap_chain_modify_wd(
	ImGui_ImplVulkanH_Window* wd,
	VkImage* backbuffers, int i )
{
	ImGui_ImplVulkanH_CreateSwapChainModifyWd(wd, backbuffers, i);
}

void im_gui_impl_vulkan_h_create_window_render_pass(
	VkDevice device, ImGui_ImplVulkanH_Window* wd,
	const VkAllocationCallbacks* allocator )
{
	ImGui_ImplVulkanH_CreateWindowRenderPass(device, wd, allocator);
}

void im_gui_impl_vulkan_h_create_window_image_views(
	VkDevice device, ImGui_ImplVulkanH_Window* wd,
	const VkAllocationCallbacks* allocator)
{
	ImGui_ImplVulkanH_CreateWindowImageViews(device, wd, allocator);
}

void im_gui_impl_vulkan_h_create_window_framebuffer(
	VkDevice device, ImGui_ImplVulkanH_Window* wd,
	const VkAllocationCallbacks* allocator )
{
	ImGui_ImplVulkanH_CreateWindowFramebuffer(device, wd, allocator);
}

VkRenderPass*
im_gui_impl_vulkan_h_create_window_render_pass_raw(
	VkDevice device, const VkAllocationCallbacks* allocator,
	bool udr, VkFormat fmt, bool ce )
{
	return ImGui_ImplVulkanH_CreateWindowRenderPassRaw(
		device, allocator, udr, fmt, ce );
}

void im_gui_impl_vulkan_h_set_wd_render_pass(
	ImGui_ImplVulkanH_Window* wd, VkRenderPass *rp )
{
	ImGui_ImplVulkanH_SetWdRenderPass(wd, rp);
}

VkImageView* im_gui_impl_vulkan_h_create_window_image_views_raw(
	VkDevice device, VkFormat fmt, uint32_t im_count, VkImage* imgs,
	const VkAllocationCallbacks* allocator )
{
	return ImGui_ImplVulkanH_CreateWindowImageViewsRaw(
		device, fmt, im_count, imgs, allocator );
}

void im_gui_impl_vulkan_h_copy_image_views_to_wd(
	ImGui_ImplVulkanH_Window* wd, VkImageView* views )
{
	ImGui_ImplVulkanH_CopyImageViewsToWd (wd, views);
}

VkFramebuffer* im_gui_impl_vulkan_h_create_window_framebuffer_raw(
	VkDevice device, const VkAllocationCallbacks* allocator,
	bool udr, int im_count, VkRenderPass* rp, int wdt, int hgt, VkImageView* bv
	)
{
	return ImGui_ImplVulkanH_CreateWindowFramebufferRaw(
		device, allocator, udr, im_count, rp, wdt, hgt, bv );
}

void im_gui_impl_vulkan_h_copy_framebuffer_to_wd(
	bool udr, ImGui_ImplVulkanH_Window* wd,
	int im_count, VkFramebuffer* fbs )
{
	ImGui_ImplVulkanH_CopyFramebufferToWd(udr, wd, im_count, fbs);
}
