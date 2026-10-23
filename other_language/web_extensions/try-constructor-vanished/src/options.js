const testButton = document.querySelector("#test");
const result = document.querySelector("#result");

testButton.addEventListener("click", async () => {
	console.log("TEST BUTTON CLICKED");
	const rslt = await browser.runtime.sendMessage({
		method: "buttonClicked",
		constructor: "Do I vanish?" });

	console.log("after: keys:", Object.keys(rslt));
	console.log("after: has:", Object.hasOwn(rslt, "constructor"));
	console.log("options.js: BUTTON RESULT:", rslt);

	result.textContent = [
		`keys: ${JSON.stringify(Object.keys(rslt))}`,
		`has constructor: ${Object.hasOwn(rslt, "constructor")}`,
		`result: ${JSON.stringify(rslt, null, 2)}`
	].join("\n"); });
