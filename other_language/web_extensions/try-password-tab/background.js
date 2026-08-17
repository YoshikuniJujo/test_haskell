console.log("background begin");

let resolveInput = null;
let inputTabId = null;
let sourceTabId = null;

browser.runtime.onMessage.addListener((msg, sender) => {
	if (msg.method == "openInputTab") {
		return new Promise(async (resolve) => {
			resolveInput = resolve;
			sourceTabId = sender.tab.id;

			const tab = await browser.tabs.create({
				url: browser.runtime.getURL("input.html")
			});

			inputTabId = tab.id;
		});
	}

	if (msg.method == "input") {
		console.log("input: ", msg.value);
		resolveInput?.(msg.value);
		resolveInput = null;

		if (inputTabId !== null) {
			browser.tabs.remove(inputTabId);
			inputTabId = null;
		}

		if (sourceTabId !== null) {
			browser.tabs.update(sourceTabId, { active: true });
			sourceTabId = null;
		}
	}

});
