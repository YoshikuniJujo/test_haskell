const pendingRequests = new Map();

const tryPasswordTab = {

	queryInput()
	{
		return new window.Promise(async (rs, rj) => {
			const rid = crypto.randomUUID();
			pendingRequests.set(rid, { resolve: rs, reject: rj });
			browser.runtime.sendMessage(
				{ method: "queryInput", request: rid } ); });
	}

};

browser.runtime.onMessage.addListener((m) => {
	switch (m.method) {
		case "pushInput":
			const rs = pendingRequests.get(m.request).resolve;
			if (!rs) {
				console.error(
					"invalid input request",
					{ request: m.request } );
				throw new Error("Invalid input request"); }
			pendingRequests.delete(m.request);
			rs(m.value);
			break; } });

window.wrappedJSObject.tryPasswordTab =
	cloneInto(tryPasswordTab, window, { cloneFunctions: true });
