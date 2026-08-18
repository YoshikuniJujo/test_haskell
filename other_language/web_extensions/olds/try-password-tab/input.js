const input = document.querySelector("#input");

document.querySelector("#send").addEventListener("click", () => {
	console.log("send input");

	const value = input.value;

	browser.runtime.sendMessage({
		method: "input", value
	});
});
