const testButton = document.querySelector("#test");

testButton.addEventListener("click", async () => {
	console.log("TEST BUTTON CLICKED");
	const rslt = await browser.runtime.sendMessage({
		method: "buttonClicked",
		constructor: "Do I vanish?"
	});

	console.log("keys:", Object.keys(rslt));
	console.log("has:", Object.hasOwn(rslt, "constructor"));
	console.log("options.js: BUTTON RESULT:", rslt);
});
