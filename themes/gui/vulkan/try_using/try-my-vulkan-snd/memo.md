memo
====

new
---

* [ ] repair try-storable-generic
* [ ] remove instance SizeAlignmentList Vertex from try-my-vulkan-snd
* [ ] refactoring try-gpu-vulkan about SizeAlignmentList

old
---

* [ ] rename Pointable to Pokable
* [ ] Vk.CommandBuffer.C s
    + [ ] Vk.CommandBuffer.C' s vs
    + [ ] withC :: C s -> (C' s vs -> IO a) -> IO s
* [ ] dynamic descriptor set
* [ ] texel buffer
* [ ] no type argument to command buffer
* [x] make test for AllocationCallbacks
* [x] refactor app/Main.hs: createInstance
* [x] use Text instead of ByteString in ExtensionProperties
* [x] move struct LayerProperties of Vulkan.Enemerate.Core
* [x] use Text instead of ByteString in LayerProperties
* [x] move struct ExtensionProperties and LayerProperties from Vulkan.Enumerate
* [x] repair ExtensionProperties specVersion is not ApiVersion
* [x] globalSwapChainImageFormat
* [x] globalSwapChainExtent
* [x] Vulkan.VertexInput.Enum
* [x] Vulkan.VertexInput
* [ ] move Vulkan.Enumerate.instanceExtensionProperties to Vulkan.Instance.enumerateExtensionProperties
* [ ] move Vulkan.Enumerate.instanceLayerProperties to Vulkan.Instance.enumerateLayerProperties
* [x] move Vulkan.PushConstant to Vulkan.PushConstant.Core
* [x] Vulkan.BufferView.Core
* [ ] Vulkan.BufferView.Middle
* [ ] use Vulkan.BufferView.Middle
* [ ] refactor app/try-saitan.hs
* [x] move modules Vulkan.Foo to Gpu.Vulkan.Foo
* [ ] Push Constants
* [x] Descriptor Buffer Info
	+ [x] has type argument: X
		- atom
		- list
* [x] Buffer
	+ [x] has type argument
		- \'[X]
* [x] Memory
	+ [x] has type argument
		- \'[ \'[X]]
* [x] define Gpu.Vulkan.DescriptorSet.Middle.updateSs
	+ Maybe: rename updateSs to updateDs
* [x] use Vk.DescriptorSet.M.updateSs in Gpu.Vulkan.DescriptorSet.List
* [x] use Vk.DescriptorSet.M.Write in Gpu.Vulkan.DescriptorSet.Atom
* [x] use Vk.DescriptorSet.M.updateSs in Gpu.Vulkan.DescriptorSet.Atom
* [ ] dynamic storage buffer

tmp
---

* VkBufferView
* texel buffer
* texelFetch
* imageLoad, imageStore

```
layout (binding = 1) uniform foo
```

app
---

* try-triangle-single.hs
* try-next-triangle.hs
* try-rectangle-single.hs
* try-rectangle.hs
* try-texture-single.hs
* try-texture.hs
* try-depth-buffering-single.hs
* try-depth-buffering.hs
* try-loading-models-single.hs
* try-loading-models.hs
* try-generating-mipmaps-single.hs
* try-generating-mipmaps.hs
* try-multisampling.hs
