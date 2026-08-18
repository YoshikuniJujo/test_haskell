const requestId = new URLSearchParams(location.search).get("request");

const input = document.querySelector("#input");

input.focus();

input.addEventListener("keydown", (event) => {
	if (event.key === "Enter") {
		event.preventDefault();
		browser.runtime.sendMessage({
			method: "sendInput", request: requestId, value: input.value
		});
	}
});

document.querySelector("#send").addEventListener("click", () => {
	console.log("send input");

	const value = input.value;

	browser.runtime.sendMessage({
		method: "sendInput", requestId, value
	});
});
