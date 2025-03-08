#include <cstdlib>
#include "imgui_impl_vulkan.h"
#include "imgui_impl_vulkan_helper_c.h"

#include <iostream>
using namespace std;

extern "C" void imguiImplVulkanHFrameFromC(
	ImGui_ImplVulkanH_Frame_C*, struct ImGui_ImplVulkanH_Frame*);

extern "C" void imguiImplVulkanHFrameToC(
	struct ImGui_ImplVulkanH_Frame*, ImGui_ImplVulkanH_Frame_C*);

extern "C" size_t sizeOfImguiImplVulkanHFrame();
extern "C" size_t alignofImguiImplVulkanHFrame();

extern "C" ImGui_ImplVulkanH_Frame_C*
	copyImguiImplVulkanHFrameC(ImGui_ImplVulkanH_Frame_C*);

extern "C" void freeImguiImplVulkanHFrameC(ImGui_ImplVulkanH_Frame_C*);

void imguiImplVulkanHFrameFromC(
	ImGui_ImplVulkanH_Frame_C *c, struct ImGui_ImplVulkanH_Frame *cxx )
{
	cxx->CommandPool = c->CommandPool; cxx->CommandBuffer = c->CommandBuffer;
	cxx->Fence = c->Fence;
	cxx->Backbuffer = c->Backbuffer; cxx->BackbufferView = c->BackbufferView;
	cxx->Framebuffer = c->Framebuffer;
}

void imguiImplVulkanHFrameToC(
	struct ImGui_ImplVulkanH_Frame* cxx, ImGui_ImplVulkanH_Frame_C* c)
{
	c->CommandPool = cxx->CommandPool; c->CommandBuffer = cxx->CommandBuffer;
	c->Fence = cxx->Fence;
	c->Backbuffer = cxx->Backbuffer; c->BackbufferView = cxx->BackbufferView;
	c->Framebuffer = cxx->Framebuffer;
}

size_t sizeOfImguiImplVulkanHFrame()
{
	return sizeof(struct ImGui_ImplVulkanH_Frame);
}

size_t
alignofImguiImplVulkanHFrame()
{
	return alignof(struct ImGui_ImplVulkanH_Frame);
}

ImGui_ImplVulkanH_Frame_C*
copyImguiImplVulkanHFrameC(ImGui_ImplVulkanH_Frame_C* c)
{
	ImGui_ImplVulkanH_Frame_C *p =
		(ImGui_ImplVulkanH_Frame_C *)malloc(sizeof(ImGui_ImplVulkanH_Frame_C));
	memcpy(p, c, sizeof(ImGui_ImplVulkanH_Frame_C));
	return p;
}

void
freeImguiImplVulkanHFrameC(ImGui_ImplVulkanH_Frame_C* c)
{
	cout << "freeImguiImplVulkanHFrameC" << endl;
	free(c);
}
