console.log("background begin");

let resolveInput = null;
let inputTabId = null;
let sourceTabId = null;

async function sendInput(requestId, value)
{
	const { requests = {} } = await browser.storage.session.get("requests");
	const request = requests[requestId];

	if (!request) { throw new Error(`Unknown requestId: ${requestId}`); }

	await browser.tabs.sendMessage(
		request.sourceTabId, { method: "inputResult", requestId, value }
	);
	await browser.tabs.update(request.sourceTabId, { active: true });
	await browser.tabs.remove(request.inputTabId);
	delete requests[requestId];
	await browser.storage.session.set({ requests });
}

browser.runtime.onMessage.addListener((msg, sender) => {

	if (msg.method == "openInputTab") {
		return (async () => {
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
		})();
	}

	if (msg.method == "sendInput") {
		console.log("sendInput: ", msg.value);
		return sendInput(
			msg.requestId,
			msg.value
		);
	}

});
