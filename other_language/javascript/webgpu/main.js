console.log("Hello, world!");

if (!navigator.gpu) {
	throw new Error("WebGPU is not available on this browser/device.");
}

console.log("WebGPU is available!!");
