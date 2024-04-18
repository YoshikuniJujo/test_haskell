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

app refactored
--------------

* try-triangle-single.hs
* try-triangle.hs
* try-rectangle-single.hs
* try-rectangle.hs

apps
----

* [x] try-triangle-single.hs
* [x] try-triangle.hs
* [x] try-rectangle-single.hs
* [x] try-rectangle.hs
* [x] try-texture-single.hs
* [x] try-texture.hs
* [x] try-depth-buffering-single.hs
* [x] try-depth-buffering.hs
* [x] try-loading-models-single.hs
* [x] try-loading-models.hs
* [x] try-generating-mipmaps-single.hs
* [x] try-generating-mipmaps.hs
* [x] try-multisampling-single.hs
* [x] try-multisampling.hs
* [x] try-vulkan-guide.hs
* [x] try-vulkan-guide-dynamic.hs
* [x] try-vulkan-guide-storage.hs
* [x] try-vulkan-guide-texture.hs
* [x] try-multisampling-use-groups-single.hs
* [ ] try-texture-image-manager-single.hs
* [ ] try-texture-descriptor-set-group-single.hs
* [ ] try-hello-world.hs
* [ ] try-yatteiku.hs
* [ ] try-saitan-simple.hs
* [ ] try-saitan.hs
* [ ] try-saitan-float.hs
* [ ] try-texel-buffer-float.hs
* [ ] try-texel-buffer-word32.hs
* [ ] try-texture-immutable-sampler-single.hs
* [ ] my-glslc.hs
* [ ] try-allocation-callbacks.hs
* [ ] try-copy-descriptor-set.hs
* [ ] try-dynamic-dsc-sets.hs
* [ ] try-image-buffer.hs
* [ ] try-no-dynamic-dsc-sets.hs
* [ ] try-pipeline-cache.hs
* [ ] try-query.hs
* [ ] try-shaderc.hs
* [ ] try-wavefront-obj.hs
