browser.runtime.sendMessage({ method: "contentStarted" });

const pendingRequests = new Map();

const tryPasswordTab = {

	queryInput()
	{
		return new window.Promise((rs, rj) => {
			const rid = crypto.randomUUID();
			pendingRequests.set(rid, { resolve: rs, reject: rj });
			browser.runtime.sendMessage(
				{ method: "queryPass", answer: rid } ); });
	}

};

browser.runtime.onMessage.addListener((m) => {
	switch (m.method) {
		case "pushPass": {
			const rq = pendingRequests.get(m.answer);
			if (!rq) {
				console.error(
					"invalid input answer",
					{ request: m.request } );
				throw new Error("Invalid input request"); }
			pendingRequests.delete(m.request);
			rq.resolve(m.value);
			break; }
		case "passError": {
			const rq = pendingRequests.get(m.answer);
			if (!rq) {
				console.error(
					"invalid input request",
					{ request: m.request } );
				throw new Error("Invalid input request");
			}
			pendingRequests.delete(m.request);
			rq.reject(
				new Error(`Input tab was closed for request: ${m.request}`)
				);
			break; }
		}
	});

window.wrappedJSObject.tryPasswordTab =
	cloneInto(tryPasswordTab, window, { cloneFunctions: true });
