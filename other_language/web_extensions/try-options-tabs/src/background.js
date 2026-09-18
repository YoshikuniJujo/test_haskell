console.log("BACKGROUND BEGIN");

browser.runtime.onMessage.addListener(async (m, s) => {
	console.log("background: recieve message:", m, s);
	switch (m.method) {
		case "openTab":
			console.log("background: openTab");
			const t = await browser.tabs.create({
				url: browser.runtime.getURL("options.html")
			});
			console.log("background: open tab:", t);
			break;
	}
});
