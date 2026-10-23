browser.runtime.onMessage.addListener(async (m, s) => {
	console.log("background.js: MESSAGE RECIEVED:", m, s);
	const foo = { method: "return", constructor: "Do I vanish?" };
	console.log("keys:", Object.keys(foo));
	console.log("has:", Object.hasOwn(foo, "constructor"));
	return foo;
});
