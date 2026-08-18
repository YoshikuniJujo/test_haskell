const requestId =
	new URLSearchParams(location.search).get("requestId");

const input = document.querySelector("#input2");

input.focus();

input.addEventListener("keydown", (event) => {
	if (event.key === "Enter") {
		event.preventDefault();
		browser.runtime.sendMessage({
			method: "sendInput", requestId, value: input.value
		});
	}
});

document.querySelector("#send2").addEventListener("click", () => {
	console.log("send input 2");

	const value = input.value;

	browser.runtime.sendMessage({
		method: "sendInput", requestId, value
	});
});
