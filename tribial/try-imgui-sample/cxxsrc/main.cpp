// Dear ImGui: standalone example application for Glfw + Vulkan

// Learn about Dear ImGui:
// - FAQ                  https://dearimgui.com/faq
// - Getting Started      https://dearimgui.com/getting-started
// - Documentation        https://dearimgui.com/docs (same as your local docs/ folder).
// - Introduction, links and more at the top of imgui.cpp

// Important note to the reader who wish to integrate imgui_impl_vulkan.cpp/.h in their own engine/app.
// - Common ImGui_ImplVulkan_XXX functions and structures are used to interface with imgui_impl_vulkan.cpp/.h.
//   You will use those if you want to use this rendering backend in your engine/app.
// - Helper ImGui_ImplVulkanH_XXX functions and structures are only used by this example (main.cpp) and by
//   the backend itself (imgui_impl_vulkan.cpp), but should PROBABLY NOT be used by your own engine/app code.
// Read comments in imgui_impl_vulkan.h.

#include "imgui.h"
#include "imgui_impl_glfw.h"
#include "imgui_impl_vulkan.h"
#include <stdio.h>          // printf, fprintf
#include <stdlib.h>         // abort
#define GLFW_INCLUDE_NONE
#define GLFW_INCLUDE_VULKAN
#include <GLFW/glfw3.h>

// Volk headers
#ifdef IMGUI_IMPL_VULKAN_USE_VOLK
#define VOLK_IMPLEMENTATION
#include <volk.h>
#endif

// [Win32] Our example includes a copy of glfw3.lib pre-compiled with VS2010 to maximize ease of testing and compatibility with old VS compilers.
// To link with VS2010-era libraries, VS2015+ requires linking with legacy_stdio_definitions.lib, which we do using this pragma.
// Your own project should not be affected, as you are likely to link with a newer binary of GLFW that is adequate for your version of Visual Studio.
#if defined(_MSC_VER) && (_MSC_VER >= 1900) && !defined(IMGUI_DISABLE_WIN32_FUNCTIONS)
#pragma comment(lib, "legacy_stdio_definitions")
#endif

// Data
static VkAllocationCallbacks*   g_Allocator = nullptr;
static VkPipelineCache          g_PipelineCache = VK_NULL_HANDLE;

static uint32_t                 g_MinImageCount = 2;
static bool                     g_SwapChainRebuild = false;

static void check_vk_result(VkResult err)
{
    if (err == VK_SUCCESS)
        return;
    fprintf(stderr, "[vulkan] Error: VkResult = %d\n", err);
    if (err < 0)
        abort();
}

static void CleanupVulkanWindow(VkInstance ist, VkDevice dvc, ImGui_ImplVulkanH_Window* wd)
{
    ImGui_ImplVulkanH_DestroyWindow(ist, dvc, wd, g_Allocator);
}

static void FrameRender(ImGui_ImplVulkanH_Window* wd, VkDevice dvc, VkQueue gq, ImDrawData* draw_data, bool* scr)
{
    VkSemaphore image_acquired_semaphore  = wd->FrameSemaphores[wd->SemaphoreIndex].ImageAcquiredSemaphore;
    VkSemaphore render_complete_semaphore = wd->FrameSemaphores[wd->SemaphoreIndex].RenderCompleteSemaphore;
    VkResult err = vkAcquireNextImageKHR(dvc, wd->Swapchain, UINT64_MAX, image_acquired_semaphore, VK_NULL_HANDLE, &wd->FrameIndex);
    if (err == VK_ERROR_OUT_OF_DATE_KHR || err == VK_SUBOPTIMAL_KHR)
        *scr = true;
    if (err == VK_ERROR_OUT_OF_DATE_KHR)
        return;
    if (err != VK_SUBOPTIMAL_KHR)
        check_vk_result(err);

    ImGui_ImplVulkanH_Frame* fd = &wd->Frames[wd->FrameIndex];
    {
        err = vkWaitForFences(dvc, 1, &fd->Fence, VK_TRUE, UINT64_MAX);    // wait indefinitely instead of periodically checking
        check_vk_result(err);

        err = vkResetFences(dvc, 1, &fd->Fence);
        check_vk_result(err);
    }
    {
        err = vkResetCommandPool(dvc, fd->CommandPool, 0);
        check_vk_result(err);
        VkCommandBufferBeginInfo info = {};
        info.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
        info.flags |= VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT;
        err = vkBeginCommandBuffer(fd->CommandBuffer, &info);
        check_vk_result(err);
    }
    {
        VkRenderPassBeginInfo info = {};
        info.sType = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
        info.renderPass = wd->RenderPass;
        info.framebuffer = fd->Framebuffer;
        info.renderArea.extent.width = wd->Width;
        info.renderArea.extent.height = wd->Height;
        info.clearValueCount = 1;
        info.pClearValues = &wd->ClearValue;
        vkCmdBeginRenderPass(fd->CommandBuffer, &info, VK_SUBPASS_CONTENTS_INLINE);
    }

    // Record dear imgui primitives into command buffer
    ImGui_ImplVulkan_RenderDrawData(draw_data, fd->CommandBuffer);

    // Submit command buffer
    vkCmdEndRenderPass(fd->CommandBuffer);
    {
        VkPipelineStageFlags wait_stage = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
        VkSubmitInfo info = {};
        info.sType = VK_STRUCTURE_TYPE_SUBMIT_INFO;
        info.waitSemaphoreCount = 1;
        info.pWaitSemaphores = &image_acquired_semaphore;
        info.pWaitDstStageMask = &wait_stage;
        info.commandBufferCount = 1;
        info.pCommandBuffers = &fd->CommandBuffer;
        info.signalSemaphoreCount = 1;
        info.pSignalSemaphores = &render_complete_semaphore;

        err = vkEndCommandBuffer(fd->CommandBuffer);
        check_vk_result(err);
        err = vkQueueSubmit(gq, 1, &info, fd->Fence);
        check_vk_result(err);
    }
}

static void FramePresent(ImGui_ImplVulkanH_Window* wd, VkQueue gq, bool* scr)
{
    if (*scr)
        return;
    VkSemaphore render_complete_semaphore = wd->FrameSemaphores[wd->SemaphoreIndex].RenderCompleteSemaphore;
    VkPresentInfoKHR info = {};
    info.sType = VK_STRUCTURE_TYPE_PRESENT_INFO_KHR;
    info.waitSemaphoreCount = 1;
    info.pWaitSemaphores = &render_complete_semaphore;
    info.swapchainCount = 1;
    info.pSwapchains = &wd->Swapchain;
    info.pImageIndices = &wd->FrameIndex;
    VkResult err = vkQueuePresentKHR(gq, &info);
    if (err == VK_ERROR_OUT_OF_DATE_KHR || err == VK_SUBOPTIMAL_KHR)
        *scr = true;
    if (err == VK_ERROR_OUT_OF_DATE_KHR)
        return;
    if (err != VK_SUBOPTIMAL_KHR)
        check_vk_result(err);
    wd->SemaphoreIndex = (wd->SemaphoreIndex + 1) % wd->SemaphoreCount; // Now we can use the next set of semaphores
}

extern "C" void step(
	GLFWwindow* window, VkInstance ist,
	VkPhysicalDevice phd, uint32_t qfi,
	VkDevice dvc, VkQueue gq, VkDescriptorPool dp, ImGui_ImplVulkanH_Window* wd,
	ImGuiIO* pio, ImGui_ImplVulkan_InitInfo* p_init_info,
	bool* p_show_demo_window, bool* p_show_another_window,
	float* p_clear_color, bool* pscr );
extern "C" void resizeSwapchain(
	VkInstance ist, VkPhysicalDevice phd, uint32_t qfi,
	VkDevice dvc, ImGui_ImplVulkanH_Window* wd,
	bool* pscr, int fbwdt, int fbhgt );

extern "C" ImGui_ImplVulkan_InitInfo* new_ImGui_ImplVulkan_InitInfo();

extern "C" void free_ImGui_ImplVulkan_InitInfo(ImGui_ImplVulkan_InitInfo* p);

extern "C" void initialize_ImGui_ImplVulkan_InitInfo(
	ImGui_ImplVulkan_InitInfo* p_init_info,
	VkInstance ist, VkPhysicalDevice phd, uint32_t qfi,
	VkDevice dvc, VkQueue gq, VkDescriptorPool dp, ImGui_ImplVulkanH_Window* wd );

extern "C" void cleanup (VkInstance ist, VkDevice dvc, ImGui_ImplVulkanH_Window* wd);

// Main code

ImGui_ImplVulkan_InitInfo*
new_ImGui_ImplVulkan_InitInfo()
{
	return (ImGui_ImplVulkan_InitInfo*)
		malloc(sizeof(ImGui_ImplVulkan_InitInfo));
}

void
free_ImGui_ImplVulkan_InitInfo(ImGui_ImplVulkan_InitInfo* p)
{
	free(p);
}

void
initialize_ImGui_ImplVulkan_InitInfo(
	ImGui_ImplVulkan_InitInfo* p_init_info,
	VkInstance ist, VkPhysicalDevice phd, uint32_t qfi,
	VkDevice dvc, VkQueue gq, VkDescriptorPool dp, ImGui_ImplVulkanH_Window* wd )
{
	p_init_info->ApiVersion = VK_API_VERSION_1_3;
	p_init_info->Instance = ist;
	p_init_info->PhysicalDevice = phd;
	p_init_info->Device = dvc;
	p_init_info->QueueFamily = qfi;
	p_init_info->Queue = gq;
	p_init_info->PipelineCache = g_PipelineCache;
	p_init_info->DescriptorPool = dp;
	p_init_info->RenderPass = wd->RenderPass;
	p_init_info->Subpass = 0;
	p_init_info->MinImageCount = g_MinImageCount;
	p_init_info->ImageCount = wd->ImageCount;
	p_init_info->MSAASamples = VK_SAMPLE_COUNT_1_BIT;
	p_init_info->Allocator = g_Allocator;
	p_init_info->CheckVkResultFn = check_vk_result;
}

void
cleanup (VkInstance ist, VkDevice dvc, ImGui_ImplVulkanH_Window* wd)
{
    // Cleanup
	VkResult err;
    err = vkDeviceWaitIdle(dvc);
    check_vk_result(err);
    ImGui_ImplVulkan_Shutdown();
    ImGui_ImplGlfw_Shutdown();
    ImGui::DestroyContext();

    CleanupVulkanWindow(ist, dvc, wd);
}

void
resizeSwapchain(
	VkInstance ist, VkPhysicalDevice phd, uint32_t qfi,
	VkDevice dvc, ImGui_ImplVulkanH_Window* wd,
	bool* pscr, int fbwdt, int fbhgt )
{
	ImGui_ImplVulkan_SetMinImageCount(g_MinImageCount);
	ImGui_ImplVulkanH_CreateOrResizeWindow(ist, phd, dvc, wd, qfi, g_Allocator, fbwdt, fbhgt, g_MinImageCount);
	wd->FrameIndex = 0;
	*pscr = false;
}

void
step(	GLFWwindow* window, VkInstance ist,
	VkPhysicalDevice phd, uint32_t qfi,
	VkDevice dvc, VkQueue gq, VkDescriptorPool dp, ImGui_ImplVulkanH_Window* wd,
	ImGuiIO* pio, ImGui_ImplVulkan_InitInfo* p_init_info,
	bool* p_show_demo_window, bool* p_show_another_window,
	float* p_clear_color, bool* pscr )
{

        // Start the Dear ImGui frame
        ImGui_ImplVulkan_NewFrame();
        ImGui_ImplGlfw_NewFrame();
        ImGui::NewFrame();

        // 1. Show the big demo window (Most of the sample code is in ImGui::ShowDemoWindow()! You can browse its code to learn more about Dear ImGui!).
        if (*p_show_demo_window)
            ImGui::ShowDemoWindow(p_show_demo_window);

        // 2. Show a simple window that we create ourselves. We use a Begin/End pair to create a named window.
        {
            static float f = 0.0f;
            static int counter = 0;

            ImGui::Begin("Hello, world!");                          // Create a window called "Hello, world!" and append into it.

            ImGui::Text("This is some useful text. 日本語でおk");               // Display some text (you can use a format strings too)
            ImGui::Checkbox("Demo Window", p_show_demo_window);      // Edit bools storing our window open/close state
            ImGui::Checkbox("Another Window", p_show_another_window);

            ImGui::SliderFloat("float", &f, 0.0f, 1.0f);            // Edit 1 float using a slider from 0.0f to 1.0f
            ImGui::ColorEdit3("clear color", p_clear_color); // Edit 3 floats representing a color

            if (ImGui::Button("Button"))                            // Buttons return true when clicked (most widgets return true when edited/activated)
                counter++;
            ImGui::SameLine();
            ImGui::Text("counter = %d", counter);

            ImGui::Text("Application average %.3f ms/frame (%.1f FPS)", 1000.0f / pio->Framerate, pio->Framerate);
            ImGui::End();
        }

        // 3. Show another simple window.
        if (*p_show_another_window)
        {
            ImGui::Begin("Another Window", p_show_another_window);   // Pass a pointer to our bool variable (the window will have a closing button that will clear the bool when clicked)
            ImGui::Text("Hello from another window!");
            if (ImGui::Button("Close Me"))
                *p_show_another_window = false;
            ImGui::End();
        }

        // Rendering
        ImGui::Render();
        ImDrawData* draw_data = ImGui::GetDrawData();
        const bool is_minimized = (draw_data->DisplaySize.x <= 0.0f || draw_data->DisplaySize.y <= 0.0f);
        if (!is_minimized)
        {
            wd->ClearValue.color.float32[0] = p_clear_color[0] * p_clear_color[3];
            wd->ClearValue.color.float32[1] = p_clear_color[1] * p_clear_color[3];
            wd->ClearValue.color.float32[2] = p_clear_color[2] * p_clear_color[3];
            wd->ClearValue.color.float32[3] = p_clear_color[3];
            FrameRender(wd, dvc, gq, draw_data, pscr);
            FramePresent(wd, gq, pscr);
        }
}
