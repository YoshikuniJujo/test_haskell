console.log("background begin");

let resolveInput = null;

browser.runtime.onMessage.addListener((msg) => {
	if (msg.method == "openInputTab") {
		return new Promise((resolve) => {
			resolveInput = resolve;
			browser.tabs.create({
				url: browser.runtime.getURL("input.html")
			});
		});
	}

	if (msg.method == "input") {
		console.log("input: ", msg.value);
		resolveInput?.(msg.value);
		resolveInput = null;
	}

});
