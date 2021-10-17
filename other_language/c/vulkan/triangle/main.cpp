#define GLFW_INCLUDE_VULKAN
#include <GLFW/glfw3.h>
// #include <vulkan/vulkan.h>

#include <iostream>
#include <stdexcept>
#include <cstdlib>
#include <vector>
#include <map>
#include <set>
#include <cstring>
#include <optional>
#include <cstdint>
#include <algorithm>

const uint32_t WIDTH = 800;
const uint32_t HEIGHT = 600;

const std::vector<const char*> validationLayers = {
	"VK_LAYER_KHRONOS_validation"
};

const std::vector<const char*> deviceExtensions = {
	VK_KHR_SWAPCHAIN_EXTENSION_NAME
};

#ifdef NDEBUG
	const bool enableValidationLayers = false;
#else
	const bool enableValidationLayers = true;
#endif

VkResult CreateDebugUtilsMessengerEXT(
	VkInstance instance,
	const VkDebugUtilsMessengerCreateInfoEXT* pCreateInfo,
	const VkAllocationCallbacks* pAllocator,
	VkDebugUtilsMessengerEXT* pDebugMessenger ) {
	auto func = (PFN_vkCreateDebugUtilsMessengerEXT)
		vkGetInstanceProcAddr(instance, "vkCreateDebugUtilsMessengerEXT");
	if (func != nullptr) {
		return func(instance, pCreateInfo, pAllocator, pDebugMessenger);
	} else {
		return VK_ERROR_EXTENSION_NOT_PRESENT;
	}
}

void DestroyDebugUtilsMessengerEXT(
	VkInstance instance,  VkDebugUtilsMessengerEXT debugMessenger,
	const VkAllocationCallbacks* pAllocator ) {
	auto func = (PFN_vkDestroyDebugUtilsMessengerEXT)
		vkGetInstanceProcAddr(
			instance, "vkDestroyDebugUtilsMessengerEXT" );
	if (func != nullptr) {
		func(instance, debugMessenger, pAllocator);
	}
}

class HelloTriangleApplication {
public:
	void run() {
		initWindow();
		initVulkan();
		mainLoop();
		cleanup();
	}
private:
	GLFWwindow* window;
	VkInstance instance;
	VkDebugUtilsMessengerEXT debugMessenger;
	VkPhysicalDevice physicalDevice = VK_NULL_HANDLE;
	VkDevice device;
	VkQueue graphicsQueue;
	VkSurfaceKHR surface;
	VkQueue presentQueue;
	VkSwapchainKHR swapChain;
	std::vector<VkImage> swapChainImages;
	VkFormat swapChainImageFormat;
	VkExtent2D swapChainExtent;
	std::vector<VkImageView> swapChainImageViews;

	void initWindow() {
		glfwInit();

		glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
		glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE);
		
		window = glfwCreateWindow(WIDTH, HEIGHT, "Vulkan", nullptr, nullptr);
	}

	void initVulkan() {
		createInstance();
		setupDebugMessenger();
		createSurface();
		pickPhysicalDevice();
		createLogicalDevice();
		createSwapChain();
		createImageViews();
	}

	void createImageViews() {
		swapChainImageViews.resize(swapChainImages.size());
		for (size_t i = 0; i < swapChainImages.size(); i++) {
			VkImageViewCreateInfo createInfo{};
			createInfo.sType =
				VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
			createInfo.image = swapChainImages[i];
			createInfo.viewType = VK_IMAGE_VIEW_TYPE_2D;
			createInfo.format = swapChainImageFormat;

			createInfo.components.r = VK_COMPONENT_SWIZZLE_IDENTITY;
			createInfo.components.g = VK_COMPONENT_SWIZZLE_IDENTITY;
			createInfo.components.b = VK_COMPONENT_SWIZZLE_IDENTITY;
			createInfo.components.a = VK_COMPONENT_SWIZZLE_IDENTITY;

			createInfo.subresourceRange.aspectMask =
				VK_IMAGE_ASPECT_COLOR_BIT;
			createInfo.subresourceRange.baseMipLevel = 0;
			createInfo.subresourceRange.levelCount = 1;
			createInfo.subresourceRange.baseArrayLayer = 0;
			createInfo.subresourceRange.layerCount = 1;

			if (vkCreateImageView(device, &createInfo, nullptr,
				&swapChainImageViews[i]) != VK_SUCCESS) {
				throw std::runtime_error("failed to create image views!");
			}
		}
	}

	struct SwapChainSupportDetails {
		VkSurfaceCapabilitiesKHR capabilities;
		std::vector<VkSurfaceFormatKHR> formats;
		std::vector<VkPresentModeKHR> presentModes;
	};

	void createSwapChain() {
		SwapChainSupportDetails swapChainSupport =
			querySwapChainSupport(physicalDevice);

		std::cout << swapChainSupport.capabilities.currentExtent.width << std::endl;
		std::cout << swapChainSupport.capabilities.currentExtent.height << std::endl;

		std::cout << swapChainSupport.formats.size() << std::endl;
		std::cout << swapChainSupport.formats[0].format << std::endl;
		std::cout << swapChainSupport.formats[0].colorSpace << std::endl;
		std::cout << VK_FORMAT_B8G8R8A8_SRGB << std::endl;
		std::cout << swapChainSupport.formats[1].format << std::endl;
		std::cout << swapChainSupport.formats[1].colorSpace << std::endl;
		std::cout << VK_FORMAT_B8G8R8A8_UNORM << std::endl;
		std::cout << swapChainSupport.presentModes.size() << std::endl;
		std::cout << swapChainSupport.presentModes[0] << std::endl;
		std::cout << swapChainSupport.presentModes[1] << std::endl;
		std::cout << swapChainSupport.presentModes[2] << std::endl;
		std::cout << swapChainSupport.presentModes[3] << std::endl;

		VkSurfaceFormatKHR surfaceFormat = chooseSwapSurfaceFormat(
			swapChainSupport.formats );

		std::cout << surfaceFormat.format << std::endl;
		std::cout << surfaceFormat.colorSpace << std::endl;

		VkPresentModeKHR presentMode = chooseSwapPresentMode(
			swapChainSupport.presentModes);

		std::cout << presentMode << std::endl;

		VkExtent2D extent = chooseSwapExtent(swapChainSupport.capabilities);

		std::cout << extent.width << std::endl;
		std::cout << extent.height << std::endl;

		uint32_t imageCount =
			swapChainSupport.capabilities.minImageCount + 1;

		if (	swapChainSupport.capabilities.maxImageCount > 0 &&
			imageCount > swapChainSupport.capabilities.maxImageCount
			) {
			imageCount = swapChainSupport.capabilities.maxImageCount;
		}

		VkSwapchainCreateInfoKHR createInfo{};
		createInfo.sType = VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR;
		createInfo.surface = surface;
		createInfo.minImageCount = imageCount;
		createInfo.imageFormat = surfaceFormat.format;
		createInfo.imageColorSpace = surfaceFormat.colorSpace;
		createInfo.imageExtent = extent;
		createInfo.imageArrayLayers = 1;
		createInfo.imageUsage = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT;

		QueueFamilyIndices indices = findQueueFamilies(physicalDevice);
		uint32_t queueFamilyIndices[] = {
			indices.graphicsFamily.value(),
			indices.presentFamily.value() };

		if (indices.graphicsFamily != indices.presentFamily) {
			createInfo.imageSharingMode = VK_SHARING_MODE_CONCURRENT;
			createInfo.queueFamilyIndexCount = 2;
			createInfo.pQueueFamilyIndices = queueFamilyIndices;
		} else {
			createInfo.imageSharingMode = VK_SHARING_MODE_EXCLUSIVE;
			createInfo.queueFamilyIndexCount = 0;
			createInfo.pQueueFamilyIndices = nullptr;
		}

		std::cout << VK_SHARING_MODE_EXCLUSIVE << std::endl;
		std::cout << createInfo.imageSharingMode << std::endl;

		createInfo.preTransform =
			swapChainSupport.capabilities.currentTransform;
		createInfo.compositeAlpha = VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR;

		createInfo.presentMode = presentMode;
		createInfo.clipped = VK_TRUE;

		createInfo.oldSwapchain = VK_NULL_HANDLE;

		if (vkCreateSwapchainKHR(
			device, &createInfo, nullptr, &swapChain ) !=
				VK_SUCCESS) {
			throw std::runtime_error(
				"failed to create swap chain!");
		}

		vkGetSwapchainImagesKHR(device, swapChain, &imageCount, nullptr);
		swapChainImages.resize(imageCount);
		vkGetSwapchainImagesKHR(device, swapChain, &imageCount, swapChainImages.data());

		swapChainImageFormat = surfaceFormat.format;
		swapChainExtent = extent;
	}

	SwapChainSupportDetails querySwapChainSupport(VkPhysicalDevice device) {
		SwapChainSupportDetails details;

		vkGetPhysicalDeviceSurfaceCapabilitiesKHR(
			device, surface, &details.capabilities);

		uint32_t formatCount;
		vkGetPhysicalDeviceSurfaceFormatsKHR(device, surface, &formatCount, nullptr);

		if (formatCount != 0) {
			details.formats.resize(formatCount);
			vkGetPhysicalDeviceSurfaceFormatsKHR(
				device, surface, &formatCount, details.formats.data());
		}

		uint32_t presentModeCount;
		vkGetPhysicalDeviceSurfacePresentModesKHR(
			device, surface, &presentModeCount, nullptr);

		if (presentModeCount != 0) {
			details.presentModes.resize(presentModeCount);
			vkGetPhysicalDeviceSurfacePresentModesKHR(
				device, surface, &presentModeCount, details.presentModes.data());
		}

		return details;
	}

	VkSurfaceFormatKHR chooseSwapSurfaceFormat(
		const std::vector<VkSurfaceFormatKHR> &availableFormats ) {

		for (const auto& availableFormat : availableFormats) {
			if (	availableFormat.format ==
					VK_FORMAT_B8G8R8A8_SRGB &&
				availableFormat.colorSpace ==
					VK_COLOR_SPACE_SRGB_NONLINEAR_KHR ) {
				return availableFormat;
			}
		}

		return availableFormats[0];
	}

	VkPresentModeKHR chooseSwapPresentMode(
		const std::vector<VkPresentModeKHR>& availablePresentModes ) {
		for (const auto& availablePresentMode : availablePresentModes) {
			if (availablePresentMode == VK_PRESENT_MODE_MAILBOX_KHR) {
				return availablePresentMode;
			}
		}

		return VK_PRESENT_MODE_FIFO_KHR;
	}

	VkExtent2D chooseSwapExtent(
		const VkSurfaceCapabilitiesKHR& capabilities) {
		if (capabilities.currentExtent.width != UINT32_MAX) {
			return capabilities.currentExtent;
		} else {
			int width, height;
			glfwGetFramebufferSize(window, &width, &height);

			VkExtent2D actualExtent = {
				static_cast<uint32_t>(width),
				static_cast<uint32_t>(height)
			};

			actualExtent.width = std::clamp(
				actualExtent.width, capabilities.minImageExtent.width,
				capabilities.maxImageExtent.width);
			actualExtent.height = std::clamp(
				actualExtent.height, capabilities.minImageExtent.height,
				capabilities.maxImageExtent.height);

			return actualExtent;
		}
	}

	void createSurface() {
		if (glfwCreateWindowSurface(
			instance, window, nullptr, &surface) != VK_SUCCESS) {
			throw std::runtime_error("failed to create window surface!");
		}
	}

	void createLogicalDevice() {
		QueueFamilyIndices indices = findQueueFamilies(physicalDevice);

		std::vector<VkDeviceQueueCreateInfo> queueCreateInfos;
		std::set<uint32_t> uniqueQueueFamilies = {
			indices.graphicsFamily.value(),
			indices.presentFamily.value() };

		float queuePriority = 1.0f;
		for (uint32_t queueFamily : uniqueQueueFamilies) {
			VkDeviceQueueCreateInfo queueCreateInfo{};
			queueCreateInfo.sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
			queueCreateInfo.queueFamilyIndex = queueFamily;
			queueCreateInfo.queueCount = 1;
			queueCreateInfo.pQueuePriorities = &queuePriority;
			queueCreateInfos.push_back(queueCreateInfo);
		}

		VkPhysicalDeviceFeatures deviceFeatures{};

		VkDeviceCreateInfo createInfo{};
		createInfo.sType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;

		createInfo.queueCreateInfoCount = static_cast<uint32_t>(queueCreateInfos.size());
		createInfo.pQueueCreateInfos = queueCreateInfos.data();

		createInfo.pEnabledFeatures = &deviceFeatures;
		createInfo.enabledExtensionCount = 0;

		if (enableValidationLayers) {
			createInfo.enabledLayerCount =
				static_cast<uint32_t>(validationLayers.size());
			createInfo.ppEnabledLayerNames = validationLayers.data();
		} else {
			createInfo.enabledLayerCount = 0;
		}

		createInfo.enabledExtensionCount = static_cast<uint32_t>(deviceExtensions.size());
		createInfo.ppEnabledExtensionNames = deviceExtensions.data();

		if (vkCreateDevice(physicalDevice, &createInfo, nullptr, &device) != VK_SUCCESS) {
			throw std::runtime_error("failed to create logical device!");
		}

		vkGetDeviceQueue(device, indices.graphicsFamily.value(), 0, &graphicsQueue);
		vkGetDeviceQueue(device, indices.presentFamily.value(), 0, &presentQueue);
	}

	void pickPhysicalDevice() {
		uint32_t deviceCount = 0;
		vkEnumeratePhysicalDevices(instance, &deviceCount, nullptr);
		if (deviceCount == 0) {
			throw std::runtime_error(
				"failed to find GPUs with Vulkan support!" );
		}

		std::vector<VkPhysicalDevice> devices(deviceCount);
		vkEnumeratePhysicalDevices(instance, &deviceCount, devices.data());

		std::multimap<int, VkPhysicalDevice> candidates;

		for (const auto& device : devices) {
			int score = rateDeviceSuitability(device);
			std::cout << "SCORE: " << score << std::endl;
			candidates.insert(std::make_pair(score, device));
		}

		if (candidates.rbegin()->first > 0) {
			physicalDevice = candidates.rbegin()->second;
			if (!isDeviceSuitable(physicalDevice)) {
				throw std::runtime_error("failed to find a suitable GPU!");
			}
		} else {
			throw std::runtime_error("failed to find a suitable GPU!");
		}
	}

	int rateDeviceSuitability(VkPhysicalDevice device) {
		VkPhysicalDeviceProperties deviceProperties;
		vkGetPhysicalDeviceProperties(device, &deviceProperties);

		VkPhysicalDeviceFeatures deviceFeatures;
		vkGetPhysicalDeviceFeatures(device, &deviceFeatures);

		int score = 0;

		if (deviceProperties.deviceType ==
			VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU) {
			score += 1000;
		}

		score += deviceProperties.limits.maxImageDimension2D;

		if (!deviceFeatures.geometryShader) { return 0; }

		return score;
	}

	bool isDeviceSuitable(VkPhysicalDevice device) {
		VkPhysicalDeviceProperties deviceProperties;
		vkGetPhysicalDeviceProperties(device, &deviceProperties);

		VkPhysicalDeviceFeatures deviceFeatures;
		vkGetPhysicalDeviceFeatures(device, &deviceFeatures);

		std::cout << deviceProperties.deviceName << std::endl;
		std::cout << deviceProperties.deviceType << std::endl;
		std::cout << VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU << std::endl;
		std::cout << deviceFeatures.geometryShader << std::endl;

		QueueFamilyIndices indices = findQueueFamilies(device);

		bool extensionsSupported = checkDeviceExtensionSupport(device);

		bool swapChainAdequate = false;
		if (extensionsSupported) {
			SwapChainSupportDetails swapChainSupport = querySwapChainSupport(device);
			swapChainAdequate =
				!swapChainSupport.formats.empty() &&
				!swapChainSupport.presentModes.empty();
		}

		return	(	deviceProperties.deviceType ==
					VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU ||
				deviceProperties.deviceType ==
					VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU ) &&
			deviceFeatures.geometryShader &&
			indices.isComplete() && extensionsSupported && swapChainAdequate;
	}

	bool checkDeviceExtensionSupport(VkPhysicalDevice device) {
		uint32_t extensionCount;
		vkEnumerateDeviceExtensionProperties(
			device, nullptr, &extensionCount, nullptr );
		std::vector<VkExtensionProperties> availableExtensions(extensionCount);
		vkEnumerateDeviceExtensionProperties(
			device, nullptr, &extensionCount, availableExtensions.data());

		std::set<std::string> requiredExtensions(
			deviceExtensions.begin(), deviceExtensions.end());

		for (const auto& extension : availableExtensions) {
			requiredExtensions.erase(extension.extensionName);
		}

		return requiredExtensions.empty();
	}

	struct QueueFamilyIndices {
		std::optional<uint32_t> graphicsFamily;
		std::optional<uint32_t> presentFamily;

		bool isComplete() {
			return	graphicsFamily.has_value() &&
				presentFamily.has_value();
		}
	};

	QueueFamilyIndices findQueueFamilies(VkPhysicalDevice device) {
		QueueFamilyIndices indices;

		uint32_t queueFamilyCount = 0;
		vkGetPhysicalDeviceQueueFamilyProperties(
			device, &queueFamilyCount, nullptr );

		std::vector<VkQueueFamilyProperties>
			queueFamilies(queueFamilyCount);
		vkGetPhysicalDeviceQueueFamilyProperties(
			device, &queueFamilyCount, queueFamilies.data() );

		int i = 0;
		for (const auto& queueFamily : queueFamilies) {
//			if (indices.isComplete()) { break; }
			VkBool32 presentSupport = false;
			vkGetPhysicalDeviceSurfaceSupportKHR(device, i, surface, &presentSupport);
			if (presentSupport) {
				indices.presentFamily = i;
			}
			if (queueFamily.queueFlags & VK_QUEUE_GRAPHICS_BIT) {
				indices.graphicsFamily = i;
			}
			i++;
		}

		return indices;
	}

	void populateDebugMessengerCreateInfo(
		VkDebugUtilsMessengerCreateInfoEXT& createInfo) {
		createInfo = {};
		createInfo.sType =
			VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT;
		createInfo.messageSeverity
			= VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT
			| VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
			| VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT;
		createInfo.messageType
			= VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
			| VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
			| VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT;
		createInfo.pfnUserCallback = debugCallback;
	}

	void setupDebugMessenger() {
		if (!enableValidationLayers) return;

		VkDebugUtilsMessengerCreateInfoEXT createInfo{};
		createInfo.sType =
			VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT;
			/*
		createInfo.messageSeverity
			= VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT
			| VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
			| VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT;
		createInfo.messageType
			= VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
			| VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
			| VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT;
		createInfo.pfnUserCallback = debugCallback;
		*/
		createInfo.pUserData = nullptr;
		populateDebugMessengerCreateInfo(createInfo);

		if (CreateDebugUtilsMessengerEXT(
			instance, &createInfo, nullptr, &debugMessenger )
				!= VK_SUCCESS) {
			throw std::runtime_error("failed to set up debug messenger!");
		}
	}

	void createInstance() {
		if (enableValidationLayers && !checkValidationLayerSupport()) {
			throw std::runtime_error(
				"validation layers requested, but not available!" );
		}

		VkApplicationInfo appInfo{};
		appInfo.sType = VK_STRUCTURE_TYPE_APPLICATION_INFO;
		appInfo.pApplicationName = "Hello Triangle";
		appInfo.applicationVersion = VK_MAKE_VERSION(1, 0, 0);
		appInfo.pEngineName = "No Engine";
		appInfo.engineVersion = VK_MAKE_VERSION(1, 0, 0);
		appInfo.apiVersion = VK_API_VERSION_1_0;

		VkInstanceCreateInfo createInfo{};
		createInfo.sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
		createInfo.pApplicationInfo = &appInfo;

		uint32_t glfwExtensionCount = 0;
		const char** glfwExtensions;

		glfwExtensions = glfwGetRequiredInstanceExtensions(&glfwExtensionCount);

		std::cout << "glfwExtensions:" << std::endl;
		for (int i = 0; i < glfwExtensionCount; i++) {
			std::cout << '\t' << glfwExtensions[i] << std::endl;
		}

		auto extensions = getRequiredExtensions();
		createInfo.enabledExtensionCount = static_cast<uint32_t>(extensions.size());
		createInfo.ppEnabledExtensionNames = extensions.data();

		VkDebugUtilsMessengerCreateInfoEXT debugCreateInfo{};
		if (enableValidationLayers) {
			createInfo.enabledLayerCount = static_cast<uint32_t>(validationLayers.size());
			createInfo.ppEnabledLayerNames = validationLayers.data();
			populateDebugMessengerCreateInfo(debugCreateInfo);
			createInfo.pNext = (VkDebugUtilsMessengerCreateInfoEXT*) &debugCreateInfo;
		} else {
			createInfo.enabledLayerCount = 0;
			createInfo.pNext = nullptr;
		}

		if (vkCreateInstance(&createInfo, nullptr, &instance) != VK_SUCCESS) {
			throw std::runtime_error("failed to create instance!");
		}

/*
		uint32_t extensionCount = 0;
		vkEnumerateInstanceExtensionProperties(nullptr, &extensionCount, nullptr);

		std::cout << "extensionCount = " << extensionCount << std::endl;

		std::vector<VkExtensionProperties> extensions(extensionCount);
		vkEnumerateInstanceExtensionProperties(nullptr, &extensionCount, extensions.data());

		std::cout << "available extensions:\n";

		for (const auto& extension : extensions) {
			std::cout << '\t' << extension.extensionName << '\n';
		}
		*/

	}

	bool checkValidationLayerSupport() {
		uint32_t layerCount;
		vkEnumerateInstanceLayerProperties(&layerCount, nullptr);

		std::vector<VkLayerProperties> availableLayers(layerCount);
		vkEnumerateInstanceLayerProperties(&layerCount, availableLayers.data());

		for (const char* layerName : validationLayers) {
			bool layerFound = false;

			for (const auto& layerProperties : availableLayers) {
				std::cout << layerProperties.layerName << std::endl;
				if (strcmp(layerName, layerProperties.layerName) == 0) {
					std::cout << "check it now" << std::endl;
					layerFound = true;
					break;
				}
			}

			if (!layerFound) {
				return false;
			}
		}

		return true;
	}

	std::vector<const char*> getRequiredExtensions() {
		uint32_t glfwExtensionCount = 0;
		const char** glfwExtensions;
		glfwExtensions =
			glfwGetRequiredInstanceExtensions(&glfwExtensionCount);

		std::vector<const char*> extensions(
			glfwExtensions, glfwExtensions + glfwExtensionCount);

		if (enableValidationLayers) {
			extensions.push_back(VK_EXT_DEBUG_UTILS_EXTENSION_NAME);
		}

		return extensions;
	}

	static VKAPI_ATTR VkBool32 VKAPI_CALL debugCallback(
		VkDebugUtilsMessageSeverityFlagBitsEXT messageSeverity,
		VkDebugUtilsMessageTypeFlagsEXT messageType,
		const VkDebugUtilsMessengerCallbackDataEXT* pCallbackData,
		void* pUserData ) {
			std::cerr << "validation layer: " << pCallbackData->pMessage << std::endl;
			return VK_FALSE;
		}

	void mainLoop() {
		while (!glfwWindowShouldClose(window)) {
			glfwPollEvents();
		}
	}

	void cleanup() {
		for (auto imageView : swapChainImageViews) {
			vkDestroyImageView(device, imageView, nullptr);
		}
		vkDestroySwapchainKHR(device, swapChain, nullptr);
		vkDestroyDevice(device, nullptr);
		if (enableValidationLayers) {
			DestroyDebugUtilsMessengerEXT(instance, debugMessenger, nullptr);
		}
		vkDestroySurfaceKHR(instance, surface, nullptr);
		vkDestroyInstance(instance, nullptr);
		glfwDestroyWindow(window);
		glfwTerminate();
	}
};

int main()
{
	HelloTriangleApplication app;

	try {
		app.run();
	} catch (const std::exception& e) {
		std::cerr << e.what() << std::endl;
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}


