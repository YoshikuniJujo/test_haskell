const requestId =
	new URLSearchParams(location.search).get("requestId");

console.log("requestId: ", requestId);

document.querySelector("#send2").addEventListener("click", () => {
	console.log("send input 2");

	const value = document.querySelector("#input2").value;

	browser.runtime.sendMessage({
		method: "sendInput", requestId, value
	});
});
