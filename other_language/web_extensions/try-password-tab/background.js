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

	if (msg.method == "openInputTab2") {
		return new Promise(async (resolve) => {
			const requestId = msg.requestId;
			const sourceTabId = sender.tab.id;

			const tab = await browser.tabs.create({
				url: browser.runtime.getURL(
					`input.html?requestId=${encodeURIComponent(requestId)}`)
			});

			const { requests = {} } =
				await browser.storage.session.get("requests");

			requests[requestId] = {
				sourceTabId,
				inputTabId: tab.id,
				state: "pending"
			};

			await browser.storage.session.set({ requests });

			console.log(
				await browser.storage.session.get("requests")
			);
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
