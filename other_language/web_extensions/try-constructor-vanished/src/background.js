browser.runtime.onMessage.addListener(async (m, s) => {
	console.log("background.js: MESSAGE RECIEVED:", m, s);
	const rtn = { method: "return", constructor: "Do I vanish?" };
	console.log("before: keys:", Object.keys(rtn));
	console.log("befere: has:", Object.hasOwn(rtn, "constructor"));
	return rtn;
});
