{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String
import Control.Monad
import Data.Foldable
import Data.Traversable
import Data.Bits
import Data.Maybe
import Data.List
import Data.Bool

import qualified Data.Set as St
import qualified Graphics.Vulkan as Vk
import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Core_1_2 as Vk
import qualified Graphics.Vulkan.Ext.VK_EXT_debug_utils as Vk
import qualified Graphics.Vulkan.Ext.VK_KHR_surface as Vk
import qualified Graphics.Vulkan.Ext.VK_KHR_swapchain as Vk
import qualified Graphics.UI.GLFW as Glfw

import Lib
import ThEnv

import qualified ReadFile as R

validationLayers :: [String]
validationLayers = [
	"VK_LAYER_KHRONOS_validation"
	]

deviceExtensions :: [String]
deviceExtensions = [
	"VK_KHR_swapchain"
	]

withCStringArray :: [String] -> (Ptr CString -> IO a) -> IO a
withCStringArray ss f = do
	css <- newCString `mapM` ss
	withArray css f <* free `mapM_` css

enableValidationLayers :: Bool
enableValidationLayers =
	maybe True (const False) $(lookupCompileEnvExp "NDEBUG")

main :: IO ()
main = do
	print enableValidationLayers
	w <- initWindow
	(i, dm, d, gq, pq, sfc, sc, scis, scif, sce, scivs, pllo, rp, gpl, scfbs, cp,
		cbs, ias, rfs) <- initVulkan w
	mainLoop w d gq pq sc cbs ias rfs
	cleanup w i dm d sfc sc scivs pllo rp gpl scfbs cp ias rfs

initWindow :: IO Glfw.Window
initWindow = do
	True <- Glfw.init
	Glfw.windowHint $ Glfw.WindowHint'ClientAPI Glfw.ClientAPI'NoAPI
	Glfw.windowHint $ Glfw.WindowHint'Resizable False
	Just w <- Glfw.createWindow 800 600 "Vulkan" Nothing Nothing
	pure w

initVulkan :: Glfw.Window -> IO (
	Vk.VkInstance, Ptr Vk.VkDebugUtilsMessengerEXT, Vk.VkDevice, Vk.VkQueue, Vk.VkQueue,
	Vk.VkSurfaceKHR, Vk.VkSwapchainKHR, [Vk.VkImage], Vk.VkFormat,
	Vk.VkExtent2D, [Vk.VkImageView], Vk.VkPipelineLayout, Vk.VkRenderPass,
	Vk.VkPipeline, [Vk.VkFramebuffer], Vk.VkCommandPool,
	Ptr Vk.VkCommandBuffer, Vk.VkSemaphore, Vk.VkSemaphore
	)
initVulkan w = do
	checkExtensionSupport
	i <- createInstance
	dm <- setupDebugMessenger i
	sfc <- createSurface i w
	pd <- pickPhysicalDevice i sfc
	(d, gq, pq) <- createLogicalDevice pd sfc
	(sc, scis, scif, sce) <- createSwapChain w pd d sfc
	scivs <- createImageViews d scis scif
	rp <- createRenderPass d scif
	(pllo, gpl) <- createGraphicsPipeline d sce rp
	scfbs <- createFrameBuffers d rp sce scivs
	cp <- createCommandPool d pd sfc
	cbs <- createCommandBuffers d sce rp gpl scfbs cp
	(ias, rfs) <- createSemaphores d
	pure (	i, dm, d, gq, pq, sfc, sc, scis, scif, sce, scivs, pllo, rp, gpl, scfbs,
		cp, cbs, ias, rfs )

createSemaphores :: Vk.VkDevice -> IO (Vk.VkSemaphore, Vk.VkSemaphore)
createSemaphores d = do
	semaphoreInfo :: Vk.VkSemaphoreCreateInfo <- Vk.newVkData \p -> do
		Vk.clearStorable p
		Vk.writeField @"sType" p Vk.VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
	imageAvailableSemaphore <- alloca \p -> do
		Vk.VK_SUCCESS <- Vk.vkCreateSemaphore d
			(Vk.unsafePtr semaphoreInfo) nullPtr p
		peek p
	renderFinishedSemaphore <- alloca \p -> do
		Vk.VK_SUCCESS <- Vk.vkCreateSemaphore d
			(Vk.unsafePtr semaphoreInfo) nullPtr p
		peek p
	pure (imageAvailableSemaphore, renderFinishedSemaphore)

createCommandBuffers ::
	Vk.VkDevice -> Vk.VkExtent2D ->
	Vk.VkRenderPass -> Vk.VkPipeline -> [Vk.VkFramebuffer] -> Vk.VkCommandPool ->
	IO (Ptr Vk.VkCommandBuffer)
createCommandBuffers d sce rp gpl scfbs cp = do
	commandBuffers :: Ptr Vk.VkCommandBuffer <- mallocArray $ length scfbs
	allocaInfo :: Vk.VkCommandBufferAllocateInfo <- Vk.newVkData \p -> do
		Vk.clearStorable p
		Vk.writeField @"sType" p
			Vk.VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
		Vk.writeField @"commandPool" p cp
		Vk.writeField @"level" p Vk.VK_COMMAND_BUFFER_LEVEL_PRIMARY
		Vk.writeField @"commandBufferCount" p . fromIntegral $ length scfbs
	Vk.VK_SUCCESS <- Vk.vkAllocateCommandBuffers d (Vk.unsafePtr allocaInfo) commandBuffers
	forArrayList_ commandBuffers scfbs $ beginCommandBuffer sce rp gpl
	pure commandBuffers

forArrayList_ :: Storable a => Ptr a -> [b] -> (a -> b -> IO ()) -> IO ()
forArrayList_ _ [] _ = pure ()
forArrayList_ p (x : xs) f = do
	(`f` x) =<< peek p
	forArrayList_ (p `advancePtr` 1) xs f

beginCommandBuffer ::
	Vk.VkExtent2D -> Vk.VkRenderPass -> Vk.VkPipeline ->
	Vk.VkCommandBuffer -> Vk.VkFramebuffer -> IO ()
beginCommandBuffer sce rp gpl cb scfb = do
	beginInfo :: Vk.VkCommandBufferBeginInfo <- Vk.newVkData \p -> do
		Vk.clearStorable p
		Vk.writeField @"sType" p
			Vk.VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
		Vk.writeField @"flags" p
			$ Vk.VkCommandBufferUsageBitmask 0
		Vk.writeField @"pInheritanceInfo" p nullPtr
	Vk.VK_SUCCESS <- Vk.vkBeginCommandBuffer cb (Vk.unsafePtr beginInfo)

	renderAreaOffset :: Vk.VkOffset2D <- Vk.newVkData \p -> do
		Vk.clearStorable p
		Vk.writeField @"x" p 0
		Vk.writeField @"y" p 0

	renderArea :: Vk.VkRect2D <- Vk.newVkData \p -> do
		Vk.clearStorable p
		Vk.writeField @"offset" p renderAreaOffset
		Vk.writeField @"extent" p sce

	clearColorValue :: Vk.VkClearColorValue <- Vk.newVkData \p -> do
		Vk.clearStorable p
		Vk.writeFieldArrayUnsafe @"float32" 0 p 0
		Vk.writeFieldArrayUnsafe @"float32" 1 p 0
		Vk.writeFieldArrayUnsafe @"float32" 2 p 0
		Vk.writeFieldArrayUnsafe @"float32" 3 p 1

	clearColor :: Vk.VkClearValue <- Vk.newVkData \p -> do
		Vk.clearStorable p
		Vk.writeField @"color" p clearColorValue

	renderPassInfo :: Vk.VkRenderPassBeginInfo <- Vk.newVkData \p -> do
		Vk.clearStorable p
		Vk.writeField @"sType" p
			Vk.VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
		Vk.writeField @"renderPass" p rp
		Vk.writeField @"framebuffer" p scfb
		Vk.writeField @"renderArea" p renderArea
		Vk.writeField @"clearValueCount" p 1
		Vk.writeField @"pClearValues" p $ Vk.unsafePtr clearColor

	Vk.vkCmdBeginRenderPass cb
		(Vk.unsafePtr renderPassInfo) Vk.VK_SUBPASS_CONTENTS_INLINE
	Vk.vkCmdBindPipeline cb Vk.VK_PIPELINE_BIND_POINT_GRAPHICS gpl
	Vk.vkCmdDraw cb 3 1 0 0
	Vk.vkCmdEndRenderPass cb
	Vk.VK_SUCCESS <- Vk.vkEndCommandBuffer cb
	pure ()

index :: Storable a => Ptr a -> Int -> Ptr a
index = advancePtr

createCommandPool ::
	Vk.VkDevice -> Vk.VkPhysicalDevice -> Vk.VkSurfaceKHR -> IO Vk.VkCommandPool
createCommandPool d pd sfc = do
	queueFamilyIndices <- findQueueFamilies pd sfc

	poolInfo :: Vk.VkCommandPoolCreateInfo <- Vk.newVkData \p -> do
		Vk.clearStorable p
		Vk.writeField @"sType" p
			Vk.VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
		Vk.writeField @"queueFamilyIndex" p . fromJust
			$ graphicsFamily queueFamilyIndices
		Vk.writeField @"flags" p $ Vk.VkCommandPoolCreateBitmask 0
	alloca \p -> do
		Vk.VK_SUCCESS <- Vk.vkCreateCommandPool d
			(Vk.unsafePtr poolInfo) nullPtr p
		peek p

createFrameBuffers ::
	Vk.VkDevice -> Vk.VkRenderPass -> Vk.VkExtent2D -> [Vk.VkImageView] ->
	IO [Vk.VkFramebuffer]
createFrameBuffers d rp sce = mapM $ createFrameBuffer d rp sce

createFrameBuffer ::
	Vk.VkDevice -> Vk.VkRenderPass -> Vk.VkExtent2D ->
	Vk.VkImageView -> IO Vk.VkFramebuffer
createFrameBuffer d rp sce iv = withArray [iv] \atms -> do
	frameBufferInfo :: Vk.VkFramebufferCreateInfo <- Vk.newVkData \p -> do
		Vk.clearStorable p
		Vk.writeField @"sType" p Vk.VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
		Vk.writeField @"renderPass" p rp
		Vk.writeField @"attachmentCount" p 1
		Vk.writeField @"pAttachments" p atms
		Vk.writeField @"width" p =<< Vk.readField @"width" (Vk.unsafePtr sce)
		Vk.writeField @"height" p =<< Vk.readField @"height" (Vk.unsafePtr sce)
		Vk.writeField @"layers" p 1
	alloca \p -> do
		Vk.VK_SUCCESS <- Vk.vkCreateFramebuffer d
			(Vk.unsafePtr frameBufferInfo) nullPtr p
		peek p

createRenderPass :: Vk.VkDevice -> Vk.VkFormat -> IO Vk.VkRenderPass
createRenderPass d scif = do
	colorAttachment :: Vk.VkAttachmentDescription <- Vk.newVkData \p -> do
		Vk.clearStorable p
		Vk.writeField @"format" p scif
		Vk.writeField @"samples" p Vk.VK_SAMPLE_COUNT_1_BIT
		Vk.writeField @"loadOp" p Vk.VK_ATTACHMENT_LOAD_OP_CLEAR
		Vk.writeField @"storeOp" p Vk.VK_ATTACHMENT_STORE_OP_STORE
		Vk.writeField @"stencilLoadOp" p Vk.VK_ATTACHMENT_LOAD_OP_DONT_CARE
		Vk.writeField @"stencilStoreOp" p Vk.VK_ATTACHMENT_STORE_OP_DONT_CARE
		Vk.writeField @"initialLayout" p Vk.VK_IMAGE_LAYOUT_UNDEFINED
		Vk.writeField @"finalLayout" p Vk.VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
	colorAttachmentRef :: Vk.VkAttachmentReference <- Vk.newVkData \p -> do
		Vk.clearStorable p
		Vk.writeField @"attachment" p 0
		Vk.writeField @"layout" p Vk.VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
	subpass :: Vk.VkSubpassDescription <- Vk.newVkData \p -> do
		Vk.clearStorable p
		Vk.writeField @"pipelineBindPoint" p Vk.VK_PIPELINE_BIND_POINT_GRAPHICS
		Vk.writeField @"colorAttachmentCount" p 1
		Vk.writeField @"pColorAttachments" p $ Vk.unsafePtr colorAttachmentRef
	dependency :: Vk.VkSubpassDependency <- Vk.newVkData \p -> do
		Vk.clearStorable p
		Vk.writeField @"srcSubpass" p Vk.VK_SUBPASS_EXTERNAL
		Vk.writeField @"dstSubpass" p 0
		Vk.writeField @"srcStageMask" p
			Vk.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
		Vk.writeField @"srcAccessMask" p $ Vk.VkAccessBitmask 0
		Vk.writeField @"dstStageMask" p
			Vk.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
		Vk.writeField @"dstAccessMask" p
			Vk.VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT
	renderPassInfo :: Vk.VkRenderPassCreateInfo <- Vk.newVkData \p -> do
		Vk.clearStorable p
		Vk.writeField @"sType" p Vk.VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
		Vk.writeField @"attachmentCount" p 1
		Vk.writeField @"pAttachments" p $ Vk.unsafePtr colorAttachment
		Vk.writeField @"subpassCount" p 1
		Vk.writeField @"pSubpasses" p $ Vk.unsafePtr subpass
		Vk.writeField @"dependencyCount" p 1
		Vk.writeField @"pDependencies" p $ Vk.unsafePtr dependency
	alloca \p -> do
		Vk.VK_SUCCESS <- Vk.vkCreateRenderPass d
			(Vk.unsafePtr renderPassInfo) nullPtr p
		peek p

createGraphicsPipeline :: Vk.VkDevice -> Vk.VkExtent2D -> Vk.VkRenderPass -> IO (Vk.VkPipelineLayout, Vk.VkPipeline)
createGraphicsPipeline d sce rp = do
	vertShaderCode <- R.readFile "shaders/vert.spv"
	fragShaderCode <- R.readFile "shaders/frag.spv"

	vertShaderModule <- createShaderModule d vertShaderCode
	fragShaderModule <- createShaderModule d fragShaderCode

	print vertShaderModule

	vertShaderStageInfo :: Vk.VkPipelineShaderStageCreateInfo <-
		Vk.newVkData \p -> do
			Vk.clearStorable p
			Vk.writeField @"sType" p
				Vk.VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
			Vk.writeField @"stage" p Vk.VK_SHADER_STAGE_VERTEX_BIT
			Vk.writeField @"module" p vertShaderModule
			withCString "main" $ Vk.writeField @"pName" p
	fragShaderStageInfo :: Vk.VkPipelineShaderStageCreateInfo <-
		Vk.newVkData \p -> do
			Vk.clearStorable p
			Vk.writeField @"sType" p
				Vk.VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
			Vk.writeField @"stage" p Vk.VK_SHADER_STAGE_FRAGMENT_BIT
			Vk.writeField @"module" p fragShaderModule
			withCString "main" $ Vk.writeField @"pName" p
	let	shaderStages = [vertShaderStageInfo, fragShaderStageInfo]

	vertexInputInfo :: Vk.VkPipelineVertexInputStateCreateInfo <-
		Vk.newVkData \p -> do
			Vk.clearStorable p
			Vk.writeField @"sType" p
				Vk.VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
			Vk.writeField @"vertexBindingDescriptionCount" p 0
			Vk.writeField @"pVertexBindingDescriptions" p nullPtr
			Vk.writeField @"vertexAttributeDescriptionCount" p 0
			Vk.writeField @"pVertexAttributeDescriptions" p nullPtr

	inputAssembly :: Vk.VkPipelineInputAssemblyStateCreateInfo <-
		Vk.newVkData \p -> do
			Vk.clearStorable p
			Vk.writeField @"sType" p
				Vk.VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
			Vk.writeField @"topology" p
				Vk.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
			Vk.writeField @"primitiveRestartEnable" p Vk.VK_FALSE
	viewport :: Vk.VkViewport <- Vk.newVkData \p -> do
		Vk.clearStorable p
		Vk.writeField @"x" p 0
		Vk.writeField @"y" p 0
		Vk.writeField @"width" p . realToFrac =<< Vk.readField @"width" (Vk.unsafePtr sce)
		Vk.writeField @"height" p . realToFrac =<< Vk.readField @"height" (Vk.unsafePtr sce)
		Vk.writeField @"minDepth" p 0
		Vk.writeField @"maxDepth" p 1

	pos :: Vk.VkOffset2D <- Vk.newVkData \p -> do
		Vk.writeField @"x" p 0
		Vk.writeField @"y" p 0
	scissor :: Vk.VkRect2D <- Vk.newVkData \p -> do
		Vk.writeField @"offset" p pos
		Vk.writeField @"extent" p sce

	viewportState :: Vk.VkPipelineViewportStateCreateInfo <-
		Vk.newVkData \p -> do
			Vk.clearStorable p
			Vk.writeField @"sType" p
				Vk.VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
			Vk.writeField @"viewportCount" p 1
			Vk.writeField @"pViewports" p $ Vk.unsafePtr viewport
			Vk.writeField @"scissorCount" p 1
			Vk.writeField @"pScissors" p $ Vk.unsafePtr scissor

	rasterizer :: Vk.VkPipelineRasterizationStateCreateInfo <-
		Vk.newVkData \p -> do
			Vk.clearStorable p
			Vk.writeField @"sType" p
				Vk.VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
			Vk.writeField @"depthClampEnable" p Vk.VK_FALSE
			Vk.writeField @"rasterizerDiscardEnable" p Vk.VK_FALSE
			Vk.writeField @"polygonMode" p Vk.VK_POLYGON_MODE_FILL
			Vk.writeField @"lineWidth" p 1
			Vk.writeField @"cullMode" p Vk.VK_CULL_MODE_BACK_BIT
			Vk.writeField @"frontFace" p Vk.VK_FRONT_FACE_CLOCKWISE
			Vk.writeField @"depthBiasEnable" p Vk.VK_FALSE
			Vk.writeField @"depthBiasConstantFactor" p 0
			Vk.writeField @"depthBiasClamp" p 0
			Vk.writeField @"depthBiasSlopeFactor" p 0

	multisampling :: Vk.VkPipelineMultisampleStateCreateInfo <-
		Vk.newVkData \p -> do
			Vk.clearStorable p
			Vk.writeField @"sType" p
				Vk.VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
			Vk.writeField @"sampleShadingEnable" p Vk.VK_FALSE
			Vk.writeField @"rasterizationSamples" p Vk.VK_SAMPLE_COUNT_1_BIT
			Vk.writeField @"minSampleShading" p 1
			Vk.writeField @"pSampleMask" p nullPtr
			Vk.writeField @"alphaToCoverageEnable" p Vk.VK_FALSE
			Vk.writeField @"alphaToOneEnable" p Vk.VK_FALSE

	colorBlendAttachment :: Vk.VkPipelineColorBlendAttachmentState <-
		Vk.newVkData \p -> do
			Vk.clearStorable p
			Vk.writeField @"colorWriteMask" p $
				Vk.VK_COLOR_COMPONENT_R_BIT .|.
				Vk.VK_COLOR_COMPONENT_G_BIT .|.
				Vk.VK_COLOR_COMPONENT_B_BIT .|.
				Vk.VK_COLOR_COMPONENT_A_BIT
			Vk.writeField @"blendEnable" p Vk.VK_FALSE
			Vk.writeField @"srcColorBlendFactor" p Vk.VK_BLEND_FACTOR_ONE
			Vk.writeField @"dstColorBlendFactor" p Vk.VK_BLEND_FACTOR_ZERO
			Vk.writeField @"colorBlendOp" p Vk.VK_BLEND_OP_ADD
			Vk.writeField @"srcAlphaBlendFactor" p Vk.VK_BLEND_FACTOR_ONE
			Vk.writeField @"dstAlphaBlendFactor" p Vk.VK_BLEND_FACTOR_ZERO
			Vk.writeField @"alphaBlendOp" p Vk.VK_BLEND_OP_ADD

	colorBlending :: Vk.VkPipelineColorBlendStateCreateInfo <-
		Vk.newVkData \p -> do
			Vk.clearStorable p
			Vk.writeField @"sType" p
				Vk.VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
			Vk.writeField @"logicOpEnable" p Vk.VK_FALSE
			Vk.writeField @"logicOp" p Vk.VK_LOGIC_OP_COPY
			Vk.writeField @"attachmentCount" p 1
			Vk.writeField @"pAttachments" p $ Vk.unsafePtr colorBlendAttachment
			Vk.writeField @"blendConstants" p 0

	pipelineLayoutInfo :: Vk.VkPipelineLayoutCreateInfo <-
		Vk.newVkData \p -> do
			Vk.clearStorable p
			Vk.writeField @"sType" p
				Vk.VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
			Vk.writeField @"setLayoutCount" p 0
			Vk.writeField @"pSetLayouts" p nullPtr
			Vk.writeField @"pushConstantRangeCount" p 0
			Vk.writeField @"pPushConstantRanges" p nullPtr

	pipelineLayout <- alloca \p -> do
		Vk.VK_SUCCESS <- Vk.vkCreatePipelineLayout d
			(Vk.unsafePtr pipelineLayoutInfo) nullPtr p
		peek p

	gpl <- withArray shaderStages \pShaderStages -> alloca \pGpl -> do
		pipelineInfo :: Vk.VkGraphicsPipelineCreateInfo <- Vk.newVkData \p -> do
			Vk.clearStorable p
			Vk.writeField @"sType" p
				Vk.VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
			Vk.writeField @"stageCount" p 2
			Vk.writeField @"pStages" p pShaderStages
			Vk.writeField @"pVertexInputState" p $ Vk.unsafePtr vertexInputInfo
			Vk.writeField @"pInputAssemblyState" p $ Vk.unsafePtr inputAssembly
			Vk.writeField @"pViewportState" p $ Vk.unsafePtr viewportState
			Vk.writeField @"pRasterizationState" p $ Vk.unsafePtr rasterizer
			Vk.writeField @"pMultisampleState" p $ Vk.unsafePtr multisampling
			Vk.writeField @"pDepthStencilState" p nullPtr
			Vk.writeField @"pColorBlendState" p $ Vk.unsafePtr colorBlending
			Vk.writeField @"pDynamicState" p nullPtr
			Vk.writeField @"layout" p pipelineLayout
			Vk.writeField @"renderPass" p rp
			Vk.writeField @"subpass" p 0
			Vk.writeField @"basePipelineHandle" p Vk.VK_NULL_HANDLE
			Vk.writeField @"basePipelineIndex" p $ - 1
		Vk.VK_SUCCESS <- Vk.vkCreateGraphicsPipelines d
			Vk.VK_NULL_HANDLE 1 (Vk.unsafePtr pipelineInfo) nullPtr pGpl
		peek pGpl

	Vk.vkDestroyShaderModule d fragShaderModule nullPtr
	Vk.vkDestroyShaderModule d vertShaderModule nullPtr
	pure (pipelineLayout, gpl)

createShaderModule :: Vk.VkDevice -> (ForeignPtr Vk.Word32, Int) -> IO Vk.VkShaderModule
createShaderModule d (dt, sz) = do
	createInfo <- Vk.newVkData \(p :: Ptr Vk.VkShaderModuleCreateInfo) -> do
		Vk.clearStorable p
		Vk.writeField @"sType" p
			Vk.VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
		Vk.writeField @"codeSize" p $ fromIntegral sz
		withForeignPtr dt $ Vk.writeField @"pCode" p
	alloca \pShaderModule -> do
		Vk.VK_SUCCESS <- Vk.vkCreateShaderModule
			d (Vk.unsafePtr createInfo) nullPtr pShaderModule
		peek pShaderModule

createImageViews :: Vk.VkDevice -> [Vk.VkImage] -> Vk.VkFormat -> IO [Vk.VkImageView]
createImageViews d scis scif = for scis \sci -> do
	srr <- Vk.newVkData \p -> do
		Vk.writeField @"aspectMask" p Vk.VK_IMAGE_ASPECT_COLOR_BIT
		Vk.writeField @"baseMipLevel" p 0
		Vk.writeField @"levelCount" p 1
		Vk.writeField @"baseArrayLayer" p 0
		Vk.writeField @"layerCount" p 1
	createInfo :: Vk.VkImageViewCreateInfo <- Vk.newVkData \p -> do
		Vk.clearStorable p
		Vk.writeField @"sType" p
			Vk.VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
		Vk.writeField @"image" p sci
		Vk.writeField @"viewType" p Vk.VK_IMAGE_VIEW_TYPE_2D
		Vk.writeField @"format" p scif
		cmp <- Vk.readField @"components" p
		Vk.writeField @"r" (Vk.unsafePtr cmp)
			Vk.VK_COMPONENT_SWIZZLE_IDENTITY
		Vk.writeField @"g" (Vk.unsafePtr cmp)
			Vk.VK_COMPONENT_SWIZZLE_IDENTITY
		Vk.writeField @"b" (Vk.unsafePtr cmp)
			Vk.VK_COMPONENT_SWIZZLE_IDENTITY
		Vk.writeField @"a" (Vk.unsafePtr cmp)
			Vk.VK_COMPONENT_SWIZZLE_IDENTITY
		Vk.writeField @"subresourceRange" p srr
	alloca \pSwapChainImageView -> do
		Vk.VK_SUCCESS <- Vk.vkCreateImageView
			d (Vk.unsafePtr createInfo) nullPtr pSwapChainImageView
		peek pSwapChainImageView

createSwapChain ::
	Glfw.Window -> Vk.VkPhysicalDevice -> Vk.VkDevice -> Vk.VkSurfaceKHR ->
	IO (Vk.VkSwapchainKHR, [Vk.VkImage], Vk.VkFormat, Vk.VkExtent2D)
createSwapChain win pd d sfc = do
	swapChainSupport <- querySwapChainSupport pd sfc
	surfaceFormat <- chooseSwapSurfaceFormat $ formats swapChainSupport
	let	presentMode =
			chooseSwapPresentMode $ presentModes swapChainSupport
	extent <- chooseSwapExtent win $ capabilities swapChainSupport
	let	cap = capabilities swapChainSupport
	imageCountMin <- Vk.readField @"minImageCount" (Vk.unsafePtr cap)
	imageCountMax <- Vk.readField @"maxImageCount" $ Vk.unsafePtr cap
	let	imageCount = bool
			(imageCountMin + 1)
			imageCountMax
			(imageCountMax > 0 && imageCountMin + 1 > imageCountMax)
	Vk.touchVkData cap
	print imageCount
	createInfo :: Vk.VkSwapchainCreateInfoKHR <- Vk.newVkData \p -> do
		Vk.clearStorable p
		Vk.writeField @"sType" p
			Vk.VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
		Vk.writeField @"surface" p sfc
		Vk.writeField @"minImageCount" p imageCount
		Vk.writeField @"imageFormat" p =<<
			Vk.readField @"format" (Vk.unsafePtr surfaceFormat)
		Vk.writeField @"imageColorSpace" p =<<
			Vk.readField @"colorSpace" (Vk.unsafePtr surfaceFormat)
		Vk.touchVkData surfaceFormat
		Vk.writeField @"imageExtent" p extent
		Vk.writeField @"imageArrayLayers" p 1
		Vk.writeField @"imageUsage" p
			Vk.VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
		indices <- findQueueFamilies pd sfc
		putStrLn "INDICES: "
		putStrLn . ('\t' :) . show $ graphicsFamily indices
		putStrLn . ('\t' :) . show $ presentFamily indices
		if graphicsFamily indices /= presentFamily indices
		then do	Vk.writeField @"imageSharingMode" p
				Vk.VK_SHARING_MODE_CONCURRENT
			Vk.writeField @"queueFamilyIndexCount" p 2
			withArray (
				maybeToList (graphicsFamily indices) ++
				maybeToList (presentFamily indices) ) \pis ->
				Vk.writeField @"pQueueFamilyIndices" p pis
		else do	Vk.writeField @"imageSharingMode" p
				Vk.VK_SHARING_MODE_EXCLUSIVE
			Vk.writeField @"queueFamilyIndexCount" p 0
			Vk.writeField @"pQueueFamilyIndices" p nullPtr
		Vk.writeField @"preTransform" p
			=<< Vk.readField @"currentTransform" (
				Vk.unsafePtr $ capabilities swapChainSupport )
		Vk.writeField @"compositeAlpha" p
			Vk.VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
		Vk.writeField @"presentMode" p presentMode
		Vk.writeField @"clipped" p Vk.VK_TRUE
		Vk.writeField @"oldSwapchain" p Vk.VK_NULL_HANDLE
	sc <- alloca \pSwapchain -> do
		Vk.VK_SUCCESS <- Vk.vkCreateSwapchainKHR
			d (Vk.unsafePtr createInfo) nullPtr pSwapchain
		peek pSwapchain
	scis <- alloca \pn -> do
		Vk.VK_SUCCESS <- Vk.vkGetSwapchainImagesKHR d sc pn nullPtr
		n <- fromIntegral <$> peek pn
		pSwapChainImages <- mallocArray n
		Vk.VK_SUCCESS <- Vk.vkGetSwapchainImagesKHR d sc pn pSwapChainImages
		peekArray n pSwapChainImages <* free pSwapChainImages
	scif <- Vk.readField @"format" $ Vk.unsafePtr surfaceFormat
	pure (sc, scis, scif, extent)

chooseSwapSurfaceFormat :: [Vk.VkSurfaceFormatKHR] -> IO Vk.VkSurfaceFormatKHR
chooseSwapSurfaceFormat availableFormats = do
	afs <- (`filterM` availableFormats) \af -> do
		fmt <- Vk.readField @"format" (Vk.unsafePtr af)
		cs <- Vk.readField @"colorSpace" (Vk.unsafePtr af)
		pure $	fmt == Vk.VK_FORMAT_B8G8R8A8_SRGB &&
			cs == Vk.VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
	print afs
	Vk.touchVkData `mapM_` availableFormats
	pure . head $ afs ++ availableFormats

chooseSwapPresentMode :: [Vk.VkPresentModeKHR] -> Vk.VkPresentModeKHR
chooseSwapPresentMode availablePresentModes = let
	pms = filter
		(== Vk.VK_PRESENT_MODE_MAILBOX_KHR) availablePresentModes in
	head $ pms ++ [Vk.VK_PRESENT_MODE_FIFO_KHR]

chooseSwapExtent :: Glfw.Window -> Vk.VkSurfaceCapabilitiesKHR -> IO Vk.VkExtent2D
chooseSwapExtent win caps = do
	ce <- Vk.readField @"currentExtent" $ Vk.unsafePtr caps
	w <- Vk.readField @"width" $ Vk.unsafePtr ce
	print w
	mne <- Vk.readField @"minImageExtent" $ Vk.unsafePtr caps
	mnew <- Vk.readField @"width" $ Vk.unsafePtr mne
	mneh <- Vk.readField @"height" $ Vk.unsafePtr mne
	mxe <- Vk.readField @"maxImageExtent" $ Vk.unsafePtr caps
	mxew <- Vk.readField @"width" $ Vk.unsafePtr mxe
	mxeh <- Vk.readField @"height" $ Vk.unsafePtr mxe
	Vk.touchVkData caps
	if w < maxBound then pure ce else do
		(w', h) <- Glfw.getFramebufferSize win
		Vk.newVkData \p -> do
			Vk.writeField @"width" p
				$ clamp (fromIntegral w') mnew mxew
			Vk.writeField @"height" p
				$ clamp (fromIntegral h) mneh mxeh

clamp :: Ord n => n -> n -> n -> n
clamp x mn mx | x < mn = mn | x > mx = mx | otherwise = x

createSurface :: Vk.VkInstance -> Glfw.Window -> IO Vk.VkSurfaceKHR
createSurface i w = alloca \pSurface -> do
	Vk.VK_SUCCESS <- Glfw.createWindowSurface i w nullPtr pSurface
	peek pSurface

createLogicalDevice :: Vk.VkPhysicalDevice -> Vk.VkSurfaceKHR -> IO (Vk.VkDevice, Vk.VkQueue, Vk.VkQueue)
createLogicalDevice pd sfc = alloca \pQueuePriority -> do
	withCStringArray validationLayers \cvls -> withCStringArray deviceExtensions \cdes -> do
		indices <- findQueueFamilies pd sfc
		let	uniqueQueueFamilies = St.fromList [
				graphicsFamily indices, presentFamily indices ]
		putStrLn $ "uniqueQueueFamilies: " ++ show uniqueQueueFamilies
		poke pQueuePriority 1
		queueCreateInfos :: [Vk.VkDeviceQueueCreateInfo] <- for (toList uniqueQueueFamilies) \queueFamily ->
			Vk.newVkData \p -> do
				Vk.clearStorable p
				Vk.writeField @"sType" p Vk.VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
				Vk.writeField @"queueFamilyIndex" p $ fromJust queueFamily
				Vk.writeField @"queueCount" p 1
				Vk.writeField @"pQueuePriorities" p pQueuePriority
		withArray queueCreateInfos \pQueueCreateInfos -> do
			deviceFeatures :: Vk.VkPhysicalDeviceFeatures <- Vk.mallocVkData
			Vk.clearStorable $ Vk.unsafePtr deviceFeatures
			createInfo :: Vk.VkDeviceCreateInfo <- Vk.newVkData \p -> do
				Vk.clearStorable p
				Vk.writeField @"sType" p Vk.VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
				Vk.writeField @"pQueueCreateInfos" p pQueueCreateInfos
				Vk.writeField @"queueCreateInfoCount" p 1
				Vk.writeField @"pEnabledFeatures" p $ Vk.unsafePtr deviceFeatures
				Vk.writeField @"enabledExtensionCount" p . fromIntegral $ length deviceExtensions
				Vk.writeField @"ppEnabledExtensionNames" p cdes
				if enableValidationLayers
				then do	Vk.writeField @"enabledLayerCount" p . fromIntegral $ length validationLayers
					Vk.writeField @"ppEnabledLayerNames" p cvls
				else Vk.writeField @"enabledLayerCount" p 0
			Vk.touchVkData deviceFeatures
			pVkDevice <- malloc
			Vk.VK_SUCCESS <- Vk.vkCreateDevice pd (Vk.unsafePtr createInfo) nullPtr pVkDevice
			Vk.touchVkData createInfo
			pGraphicsQueue <- malloc
			device <- peek pVkDevice
			Vk.vkGetDeviceQueue device (fromJust $ graphicsFamily indices) 0 pGraphicsQueue
			pPresentQueue <- malloc
			Vk.vkGetDeviceQueue device (fromJust $ presentFamily indices) 0 pPresentQueue
			graphicsQueue <- peek pGraphicsQueue
			presentQueue <- peek pPresentQueue
			free pVkDevice
			free pGraphicsQueue
			free pPresentQueue
			pure (device, graphicsQueue, presentQueue)

pickPhysicalDevice :: Vk.VkInstance -> Vk.VkSurfaceKHR -> IO Vk.VkPhysicalDevice
pickPhysicalDevice i sfc = alloca \pn -> do
	Vk.VK_SUCCESS <- Vk.vkEnumeratePhysicalDevices i pn nullPtr
	n <- peek pn
	putStrLn $ "PHYSICAL DEVICE NUMBER: " ++ show n
	when (n == 0) $ error "failed to find GPUs with Vulkan support!"
	pDevices <- mallocArray $ fromIntegral n
	Vk.VK_SUCCESS <- Vk.vkEnumeratePhysicalDevices i pn pDevices
	ds <- peekArray (fromIntegral n) pDevices
	ds' <- (`filterM` ds) \d -> isDeviceSuitable d sfc
	when (null ds') $ error "failed to find a suitable GPU!"
	pure $ head ds'

data QueueFamilyIndices = QueueFamilyIndices {
	graphicsFamily :: Maybe Vk.Word32,
	presentFamily :: Maybe Vk.Word32,
	isComplete :: QueueFamilyIndices -> Bool
	}

queueFamilyIndices0 :: QueueFamilyIndices
queueFamilyIndices0 = QueueFamilyIndices Nothing Nothing \q ->
	isJust (graphicsFamily q) && isJust (presentFamily q)

isDeviceSuitable :: Vk.VkPhysicalDevice -> Vk.VkSurfaceKHR -> IO Bool
isDeviceSuitable d sfc = alloca \pdp -> alloca \pdf -> do
	Vk.vkGetPhysicalDeviceProperties d pdp
	print =<< peek pdp
	print =<< Vk.readField @"deviceName" pdp
	let	os = Vk.fieldOffset @"deviceName" @Vk.VkPhysicalDeviceProperties
		ln = Vk.fieldArrayLength @"deviceName" @Vk.VkPhysicalDeviceProperties
	putStrLn . ('\t' :) =<< peekCStringLen (pdp `plusPtr` os, ln)
	print =<< Vk.readField @"deviceType" pdp
	Vk.vkGetPhysicalDeviceFeatures d pdf
	print =<< peek pdf
	print =<< Vk.readField @"geometryShader" pdf

	indices <- findQueueFamilies d sfc

	extensionsSupported <- checkDeviceExtensionSupport d
	swapChainAdequate <- if not extensionsSupported then pure False else do
		swapChainSupport <- querySwapChainSupport d sfc
		pure $ not (null $ formats swapChainSupport) && not (null $ presentModes swapChainSupport)
	pure $ isComplete indices indices && extensionsSupported && swapChainAdequate

data SwapChainSupportDetails = SwapChainSupportDetails {
	capabilities :: Vk.VkSurfaceCapabilitiesKHR,
	formats :: [Vk.VkSurfaceFormatKHR],
	presentModes :: [Vk.VkPresentModeKHR] } deriving Show

querySwapChainSupport :: Vk.VkPhysicalDevice -> Vk.VkSurfaceKHR -> IO SwapChainSupportDetails
querySwapChainSupport d sfc = do
	cpbs <- alloca \pcpbs -> do
		Vk.VK_SUCCESS <- Vk.vkGetPhysicalDeviceSurfaceCapabilitiesKHR d sfc pcpbs
		peek pcpbs
	putStrLn $ "details.capabilities: " ++ show cpbs
	fmts <- alloca \pn -> do
		Vk.VK_SUCCESS <- Vk.vkGetPhysicalDeviceSurfaceFormatsKHR d sfc pn nullPtr
		n <- peek pn
		if n == 0 then pure []
		else allocaArray (fromIntegral n) \pfmts -> do
			Vk.VK_SUCCESS <- Vk.vkGetPhysicalDeviceSurfaceFormatsKHR d sfc pn pfmts
			peekArray (fromIntegral n) pfmts
	print fmts
	pms <- alloca \pn -> do
		Vk.VK_SUCCESS <- Vk.vkGetPhysicalDeviceSurfacePresentModesKHR d sfc pn nullPtr
		n <- fromIntegral <$> peek pn
		if n == 0 then pure []
		else allocaArray n \ppms -> do
			Vk.VK_SUCCESS <- Vk.vkGetPhysicalDeviceSurfacePresentModesKHR d sfc pn ppms
			peekArray n ppms
	print pms
	pure $ SwapChainSupportDetails cpbs fmts pms

checkDeviceExtensionSupport :: Vk.VkPhysicalDevice -> IO Bool
checkDeviceExtensionSupport d = alloca \pn ->  do
	Vk.VK_SUCCESS <- Vk.vkEnumerateDeviceExtensionProperties d nullPtr pn nullPtr
	n <- peek pn
	putStrLn $ "EXTENSION PROPERTY NUMBER: " ++ show n
	allocaArray (fromIntegral n) \pAvailableExtensions -> do
		Vk.VK_SUCCESS <- Vk.vkEnumerateDeviceExtensionProperties d
			nullPtr pn pAvailableExtensions
		putStrLn "EXTENSION PROPERTIES: "
		ens <- mapM ((takeWhile (/= '\NUL') <$>) . peekExtensionName)
			=<< peekArray (fromIntegral n) pAvailableExtensions
		(putStrLn . ('\t' :)) `mapM_` ens
		pure . null $ deviceExtensions \\ ens

findQueueFamilies :: Vk.VkPhysicalDevice -> Vk.VkSurfaceKHR -> IO QueueFamilyIndices
findQueueFamilies d sfc = alloca \pn -> do
	Vk.vkGetPhysicalDeviceQueueFamilyProperties d pn nullPtr
	n <- peek pn
	pqfps <- mallocArray $ fromIntegral n
	Vk.vkGetPhysicalDeviceQueueFamilyProperties d pn pqfps
	qfps <- peekArray (fromIntegral n) pqfps
	print qfps
	flags <- for qfps \qfp -> Vk.readField @"queueFlags" (Vk.unsafePtr qfp)
	print flags
	flags' <- for [0 .. n - 1] \i -> alloca \pb -> do
		Vk.VK_SUCCESS <- Vk.vkGetPhysicalDeviceSurfaceSupportKHR d i sfc pb
		peek pb
	print flags'
	pure queueFamilyIndices0 {
		graphicsFamily = fromIntegral
			<$> findIndex ((/= zeroBits) . (.&. Vk.VK_QUEUE_GRAPHICS_BIT)) flags,
		presentFamily = fromIntegral <$> findIndex (== Vk.VK_TRUE) flags'
		}

createInstance :: IO Vk.VkInstance
createInstance = do
	when enableValidationLayers do
		b <- checkValidationLayerSupport
		when (not b) $ error
			"validation layers requested, but not available!"
	withCString "Hello Triangle" \cstr_ht ->
		withCString "No Engine" \cstr_ne ->
			getRequiredExtensions \es -> withArrayLen es \n es' ->
				withCStringArray validationLayers \cvls -> do
					appInfo :: Vk.VkApplicationInfo <- Vk.newVkData \p -> do
						Vk.writeField @"sType" p Vk.VK_STRUCTURE_TYPE_APPLICATION_INFO
						Vk.writeField @"pApplicationName" p cstr_ht
						Vk.writeField @"applicationVersion" p $ Vk._VK_MAKE_VERSION 1 0 0
						Vk.writeField @"pEngineName" p cstr_ne
						Vk.writeField @"engineVersion" p $ Vk._VK_MAKE_VERSION 1 0 0
						Vk.writeField @"apiVersion" p Vk.VK_API_VERSION_1_0
					createInfo :: Vk.VkInstanceCreateInfo <- Vk.newVkData \p -> do
						Vk.writeField @"sType" p Vk.VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
						Vk.writeField @"pApplicationInfo" p $ Vk.unsafePtr appInfo
						Vk.writeField @"enabledExtensionCount" p $ fromIntegral n
						Vk.writeField @"ppEnabledExtensionNames" p es'
						if enableValidationLayers
						then do	Vk.writeField @"enabledLayerCount" p
								. fromIntegral $ length validationLayers
							Vk.writeField @"ppEnabledLayerNames" p cvls
							debugCreateInfo <- populateDebugMessengerCreateInfo
							Vk.writeField @"pNext" p . castPtr $ Vk.unsafePtr debugCreateInfo
							Vk.touchVkData debugCreateInfo
						else do	Vk.writeField @"enabledLayerCount" p 0
							Vk.writeField @"pNext" p nullPtr
					i <- malloc
					Vk.VK_SUCCESS <- Vk.vkCreateInstance (Vk.unsafePtr createInfo) nullPtr i
					Vk.touchVkData createInfo
					Vk.touchVkData appInfo
					peek i

getRequiredExtensions :: ([CString] -> IO a) -> IO a
getRequiredExtensions f = do
	extensions <- Glfw.getRequiredInstanceExtensions
	if enableValidationLayers
	then withCString "VK_EXT_debug_utils" \cs -> f (extensions ++ [cs])
	else f extensions

checkExtensionSupport :: IO ()
checkExtensionSupport = alloca \pn -> do
	Vk.VK_SUCCESS <- Vk.vkEnumerateInstanceExtensionProperties nullPtr pn nullPtr
	n <- peek pn
	(pp, ps) <- Vk.mallocVkDataArray $ fromIntegral n
	Vk.VK_SUCCESS <- Vk.vkEnumerateInstanceExtensionProperties nullPtr pn pp
	print $ length ps
	putStrLn "available extensions:"
	for_ ps \p -> putStrLn . ('\t' :) =<< peekExtensionName p

peekExtensionName :: Vk.VkExtensionProperties -> IO String
peekExtensionName p = peekCStringLen (Vk.unsafePtr p `plusPtr` os, ln)
	<* Vk.touchVkData p
	where	os = Vk.fieldOffset @"extensionName" @Vk.VkExtensionProperties
		ln = Vk.fieldArrayLength @"extensionName" @Vk.VkExtensionProperties

setupDebugMessenger :: Vk.VkInstance -> IO (Ptr Vk.VkDebugUtilsMessengerEXT)
setupDebugMessenger i = if not enableValidationLayers then pure nullPtr else do
	pDebugMessenger :: Ptr Vk.VkDebugUtilsMessengerEXT <- malloc
	vkCreateDebugUtilsMessengerEXT <- createDebugUtilsMessengerEXT i
	createInfo <- populateDebugMessengerCreateInfo
	Vk.VK_SUCCESS <- vkCreateDebugUtilsMessengerEXT i (Vk.unsafePtr createInfo) nullPtr pDebugMessenger
	Vk.touchVkData createInfo
	pure pDebugMessenger

populateDebugMessengerCreateInfo :: IO Vk.VkDebugUtilsMessengerCreateInfoEXT
populateDebugMessengerCreateInfo = do
	Vk.newVkData \p -> do
		Vk.writeField @"sType" p
			Vk.VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT
		Vk.writeField @"pNext" p nullPtr
		Vk.writeField @"messageSeverity" p $
			Vk.VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT .|.
			Vk.VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT .|.
			Vk.VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
		Vk.writeField @"messageType" p $
			Vk.VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT .|.
			Vk.VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT .|.
			Vk.VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
		Vk.writeField @"pfnUserCallback" p
			=<< wrapDebugUtilsMessengerCallbackEXT debugCallback
		Vk.writeField @"pUserData" p nullPtr

checkValidationLayerSupport :: IO Bool
checkValidationLayerSupport = alloca \pn -> do
	Vk.VK_SUCCESS <- Vk.vkEnumerateInstanceLayerProperties pn nullPtr
	n <- peek pn
	(pp, ps) <- Vk.mallocVkDataArray $ fromIntegral n
	Vk.VK_SUCCESS <- Vk.vkEnumerateInstanceLayerProperties pn pp
	print $ length ps
	vls <- for ps \p -> do
		peekCStringLen (Vk.unsafePtr p `plusPtr` os, ln)
			<* Vk.touchVkData p
	let	vls' = takeWhile (/= '\NUL') <$> vls
	print vls'
	pure . null $ validationLayers \\ vls'
	where
	os = Vk.fieldOffset @"layerName" @Vk.VkLayerProperties
	ln = Vk.fieldArrayLength @"layerName" @Vk.VkLayerProperties

debugCallback :: Vk.HS_vkDebugUtilsMessengerCallbackEXT
debugCallback _messageSeverity _messageType pCallbackData _pUserData = do
	msg <- Vk.readField @"pMessage" pCallbackData
	putStrLn . ("validation layer: " ++) =<< peekCString msg
	pure Vk.VK_FALSE

mainLoop ::
	Glfw.Window -> Vk.VkDevice ->
	Vk.VkQueue -> Vk.VkQueue ->
	Vk.VkSwapchainKHR -> Ptr Vk.VkCommandBuffer -> Vk.VkSemaphore -> Vk.VkSemaphore -> IO ()
mainLoop w d gq pq sc cbs ias rfs = do
	b <- Glfw.windowShouldClose w
	Glfw.pollEvents
	drawFrame d gq pq sc cbs ias rfs
	Vk.vkDeviceWaitIdle d
	bool (mainLoop w d gq pq sc cbs ias rfs) (pure ()) b

drawFrame ::
	Vk.VkDevice -> Vk.VkQueue -> Vk.VkQueue ->
	Vk.VkSwapchainKHR -> Ptr Vk.VkCommandBuffer -> Vk.VkSemaphore -> Vk.VkSemaphore -> IO ()
drawFrame d gq pq sc cbs ias rfs = do
	imageIndex <- alloca \p -> do
		Vk.vkAcquireNextImageKHR d sc maxBound ias Vk.VK_NULL_HANDLE p
		peek p
	waitSemaphores <- malloc
	poke waitSemaphores ias
	waitStages <- malloc
	poke waitStages Vk.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
	signalSemaphores <- malloc
	poke signalSemaphores rfs
	submitInfo :: Vk.VkSubmitInfo <- Vk.newVkData \p -> do
		Vk.clearStorable p
		Vk.writeField @"sType" p Vk.VK_STRUCTURE_TYPE_SUBMIT_INFO
		Vk.writeField @"waitSemaphoreCount" p 1
		Vk.writeField @"pWaitSemaphores" p waitSemaphores
		Vk.writeField @"pWaitDstStageMask" p waitStages
		Vk.writeField @"commandBufferCount" p 1
		Vk.writeField @"pCommandBuffers" p $ cbs `index` fromIntegral imageIndex
		Vk.writeField @"signalSemaphoreCount" p 1
		Vk.writeField @"pSignalSemaphores" p signalSemaphores
	Vk.VK_SUCCESS <- Vk.vkQueueSubmit gq 1
		(Vk.unsafePtr submitInfo) Vk.VK_NULL_HANDLE
	swapChains <- malloc
	poke swapChains sc
	pImageIndex <- malloc
	poke pImageIndex imageIndex
	presentInfo :: Vk.VkPresentInfoKHR <- Vk.newVkData \p -> do
		Vk.clearStorable p
		Vk.writeField @"sType" p
			Vk.VK_STRUCTURE_TYPE_PRESENT_INFO_KHR
		Vk.writeField @"waitSemaphoreCount" p 1
		Vk.writeField @"pWaitSemaphores" p signalSemaphores
		Vk.writeField @"swapchainCount" p 1
		Vk.writeField @"pSwapchains" p swapChains
		Vk.writeField @"pImageIndices" p pImageIndex
		Vk.writeField @"pResults" p nullPtr
	Vk.vkQueuePresentKHR pq $ Vk.unsafePtr presentInfo
	pure ()

cleanup ::
	Glfw.Window -> Vk.VkInstance -> Ptr Vk.VkDebugUtilsMessengerEXT ->
	Vk.VkDevice -> Vk.VkSurfaceKHR -> Vk.VkSwapchainKHR -> [Vk.VkImageView] ->
	Vk.VkPipelineLayout -> Vk.VkRenderPass -> Vk.VkPipeline -> [Vk.VkFramebuffer] ->
	Vk.VkCommandPool -> Vk.VkSemaphore -> Vk.VkSemaphore ->
	IO ()
cleanup w i pdm d sfc sc scivs pllo rp gpl scfbs cp ias rfs = do
	Vk.vkDestroySemaphore d rfs nullPtr
	Vk.vkDestroySemaphore d ias nullPtr
	Vk.vkDestroyCommandPool d cp nullPtr
	(\scfb -> Vk.vkDestroyFramebuffer d scfb nullPtr) `mapM_` scfbs
	Vk.vkDestroyPipeline d gpl nullPtr
	Vk.vkDestroyPipelineLayout d pllo nullPtr
	Vk.vkDestroyRenderPass d rp nullPtr
	for_ scivs \sciv -> Vk.vkDestroyImageView d sciv nullPtr
	Vk.vkDestroySwapchainKHR d sc nullPtr
	Vk.vkDestroyDevice d nullPtr
	when enableValidationLayers do
		vkDestroyDebugUtilsMessengerEXT <-
			createDestroyDebugUtilsMessengerEXT i
		peek pdm >>= \dm -> vkDestroyDebugUtilsMessengerEXT i dm nullPtr
	Vk.vkDestroySurfaceKHR i sfc nullPtr
	Vk.vkDestroyInstance i nullPtr
	Glfw.destroyWindow w
	Glfw.terminate
