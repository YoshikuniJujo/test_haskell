browser.runtime.onMessage.addListener(async (m, s) => {
	console.log("background.js: MESSAGE RECIEVED:", m, s);
	const foo = { method: "return", constructor: "Do I vanish?" };
	console.log("before: keys:", Object.keys(foo));
	console.log("befere: has:", Object.hasOwn(foo, "constructor"));
	return foo;
});
