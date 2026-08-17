document.querySelector("#send").addEventListener("click", () => {
	console.log("send input");

	const value = document.querySelector("#input").value;

	browser.runtime.sendMessage({
		method: "input", value
	});
});
