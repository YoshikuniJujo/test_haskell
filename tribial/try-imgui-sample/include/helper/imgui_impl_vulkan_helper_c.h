#include <stdbool.h>
#include <vulkan/vulkan.h>

typedef struct
{
    VkCommandPool       CommandPool;
    VkCommandBuffer     CommandBuffer;
    VkFence             Fence;
    VkImage             Backbuffer;
    VkImageView         BackbufferView;
    VkFramebuffer       Framebuffer;
} ImGui_ImplVulkanH_Frame_C;

typedef struct
{
    VkSemaphore         ImageAcquiredSemaphore;
    VkSemaphore         RenderCompleteSemaphore;
} ImGui_ImplVulkanH_FrameSemaphores_C;

typedef struct
{
    int                 Width;
    int                 Height;
    VkSwapchainKHR      Swapchain;
    VkSurfaceKHR        Surface;
    VkSurfaceFormatKHR  SurfaceFormat;
    VkPresentModeKHR    PresentMode;
    VkRenderPass        RenderPass;
    VkPipeline          Pipeline;               // The window pipeline may uses a different VkRenderPass than the one passed in ImGui_ImplVulkan_InitInfo
    bool                UseDynamicRendering;
    bool                ClearEnable;
    VkClearValue        ClearValue;
    uint32_t            FrameIndex;             // Current frame being rendered to (0 <= FrameIndex < FrameInFlightCount)
    uint32_t            ImageCount;             // Number of simultaneous in-flight frames (returned by vkGetSwapchainImagesKHR, usually derived from min_image_count)
    uint32_t            SemaphoreCount;         // Number of simultaneous in-flight frames + 1, to be able to use it in vkAcquireNextImageKHR
    uint32_t            SemaphoreIndex;         // Current set of swapchain wait semaphores we're using (needs to be distinct from per frame data)
	int						Framec;
	struct ImGui_ImplVulkanH_Frame*			pFrames;
	int						FrameSemaphorec;
	struct ImGui_ImplVulkanH_FrameSemaphores*	pFrameSemaphores;

//    ImVector<ImGui_ImplVulkanH_Frame>           Frames;
//    ImVector<ImGui_ImplVulkanH_FrameSemaphores> FrameSemaphores;
} ImGui_ImplVulkanH_Window_C;
