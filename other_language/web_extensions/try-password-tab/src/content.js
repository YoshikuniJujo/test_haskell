document.body.style.border = "3px solid green";

const pendingRequests = new Map();

const test = {

	openInputTab() {
		return new window.Promise(async (resolve, reject) => {

			const rid = crypto.randomUUID();

			pendingRequests.set(rid, { resolve, reject });

			browser.runtime.sendMessage({
				method: "queryInput",
				request: rid
			});
		});
	}

};

browser.runtime.onMessage.addListener((msg) => {
	if (msg.method !== "pushInput") { return; }

	console.log("input result: ", msg.value);

	const resolve = pendingRequests.get(msg.request).resolve;
	if (!resolve) { return; }

	pendingRequests.delete(msg.request);
	resolve(msg.value);
});

window.wrappedJSObject.test = cloneInto(test, window, { cloneFunctions: true });
