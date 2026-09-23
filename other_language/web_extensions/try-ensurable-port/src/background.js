console.log("BACKGROUND BEGIN");

browser.runtime.onMessage.addListener((m, s) => {
	console.log(m);
	console.log(s);
	console.log(s.tab);
	switch(m.method) {
		case "hello":
			console.log("background: hello");
			/*
			browser.tabs.sendMessage(s.tab.id,
				{ method: "world" } );
				*/
			const port = browser.tabs.connect(s.tab.id,
				{ name: "port" } );
			port.onDisconnect.addListener(() => {
				console.log("background.js: disconnect:", port);
			});
			console.log("background.js: port is ", port);
			console.log("background.js: port.error is ", port.error);
			port.postMessage("Foo Bar");
			break;
		case "openOptionsInTab":
			(async () => {
			console.log("background: openInTab");
			const ot = await browser.tabs.create({
				active: false,
				url: browser.runtime.getURL(
					"options.html?openType=tab&pageId=tab" )
			});
			})();
			break;
	}
});
