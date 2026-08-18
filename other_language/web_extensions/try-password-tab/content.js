document.body.style.border = "3px solid green";

const pendingRequests = new Map();

const test = {

	openInputTab() {
		return new window.Promise(async (resolve) => {

			const r = await browser.runtime.sendMessage({
				method: "openInputTab"
			});

			console.log("content result: ", r);

			resolve(r);
		});

	},

	openInputTab2() {
		return new window.Promise(async (resolve, reject) => {

			const requestId = crypto.randomUUID();

			pendingRequests.set(requestId, { resolve, reject });

			browser.runtime.sendMessage({
				method: "openInputTab2",
				requestId
			});

//			resolve("foobar");
		});
	}

};

browser.runtime.onMessage.addListener((msg) => {
	if (msg.method !== "inputResult") {
		return;
	}

	console.log("input result: ", msg.value);

	const resolve = pendingRequests.get(msg.requestId).resolve;
	if (!resolve) { return; }

	pendingRequests.delete(msg.requestId);
	resolve(msg.value);
});

window.wrappedJSObject.test =
	cloneInto(test, window, {
		cloneFunctions: true
	});
