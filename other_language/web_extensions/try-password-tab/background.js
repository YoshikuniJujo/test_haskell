console.log("background begin");

browser.runtime.onMessage.addListener((msg) => {
	if (msg.method == "openInputTab") {
		browser.tabs.create({
			url: browser.runtime.getURL("input.html")
		});
	}

});
