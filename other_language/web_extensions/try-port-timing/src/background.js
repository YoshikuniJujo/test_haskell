console.log("BACKGROUND");

browser.runtime.onMessage.addListener((m, s) => {
	switch (m.method) {
		case "timeoutTest": {
			console.log("BACKGROUND: timeoutTest");
			const listener = p => {
				console.log("BACKGROUND:", p);
				p.postMessage("HERE YOU ARE");
				console.log("BACKGROUND: after post:", p);
				p.onDisconnect.addListener(() => {
					browser.runtime.onConnect.removeListener(listener);
				});
			};
			browser.runtime.onConnect.addListener(listener);
			console.log(s.tab.id);
			browser.tabs.sendMessage(s.tab.id, { method: "timeoutTest-ack" });
			break; }
		case "postBeforeAddListener": {
			console.log("BACKGROUND: postBeforeAddListener");
			const listener = p => {
				console.log("BACKGROUND:", p);
				p.postMessage("FOOBARBAZ1 from BACKGROUND");
				p.postMessage("FOOBARBAZ2 from BACKGROUND");
				p.postMessage("FOOBARBAZ3 from BACKGROUND");
				p.postMessage("FOOBARBAZ4 from BACKGROUND");
				p.postMessage("FOOBARBAZ5 from BACKGROUND");
				p.onMessage.addListener(m => {
					console.log("BACKGROUND:", m);
				});
			};
			browser.runtime.onConnect.addListener(listener);
			browser.tabs.sendMessage(s.tab.id, { method: "postBeforeAddListener-ack" });
			break; }
		case "openOptionsInTab":
			(async () => {
				console.log("BACKGROUND: openOptionsInTab");
				await browser.tabs.create({
					active: true,
					url: browser.runtime.getURL(
						"options.html?openType=tab&pageId=tab" ) }); })();
			break;
	}
});
