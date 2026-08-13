console.log("background started");

browser.runtime.onMessage.addListener((message) => {
	console.log("background received: ", message);
	return Promise.resolve({
		anser: "backgroundから返しました"
	});
});
