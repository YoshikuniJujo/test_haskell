console.log("content script loaded as module");

document.body.style.border = "5px solid green";

(async () => {

	const result = await browser.runtime.sendMessage({
		hello: "background"
	});

	console.log("response: ", result); })();
