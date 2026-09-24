console.log("BACKGROUND BEGIN");

browser.runtime.onMessage.addListener((m, s) => {
	console.log("background.js: receive message:", m, s);
	switch(m.method) {
		case "sendMessageToMe":
			return browser.tabs.sendMessage(s.tab.id, {
				method: "messageFromBackground" } );
		case "connectToMe":
			return new Promise((rs, rj) => {
				console.log("background.js: connectToMe");
				const port = browser.tabs.connect(s.tab.id,
					{ name: "port" } );
				port.onDisconnect.addListener(() => {
					console.log(
						"background.js: disconnect:",
						port);
					console.log(
						"background.js: disconnect: " +
						"port.error:", port.error);
					if (port.error)
						rj(new Error(port.error));
					else rs();
				});
				port.onMessage.addListener(m => {
					console.log(
						"background.js: port receive:",
						m );
					port.disconnect();
					rs();
				});
				console.log("background.js: port is ", port);
				console.log(
					"background.js: port.error is ",
					port.error );
				port.postMessage("Foo Bar"); });
		case "openOptionsInTab":
			(async () => {
				console.log("background: openInTab");
				await browser.tabs.create({
					active: true,
					url: browser.runtime.getURL(
						"options.html" +
						"?openType=tab&pageId=tab" )
				}); })();
			break;
	}
});
