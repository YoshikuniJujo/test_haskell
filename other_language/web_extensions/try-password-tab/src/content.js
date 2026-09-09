browser.runtime.sendMessage({ method: "contentStarted" });

const requestsWaitingForPassword = new Map();

const tryPasswordTab = {

	getSomething(pk, prm)
	{
		return new window.Promise(async (rslv, rjct) => {
			try {
				await new Promise((rs, rj) => {
					addToArrayMap(
						requestsWaitingForPassword, pk,
						{ resolve: rs, reject: rj } );
					browser.runtime.sendMessage({
						method: "queryPswd", pubKey: pk });
				});
				rslv(await browser.runtime.sendMessage({
					method: "getSomething", pubKey: pk,
					parameter: prm }));
			}
			catch(e) { rjct(e); } });
	}

};

browser.runtime.onMessage.addListener((m) => { switch (m.method) {
	case "pswdReady":
		forEachValues(requestsWaitingForPassword,
			m.pubKey, (wtr) => wtr.resolve()); break;
	case "inputTabClosed":
		forEachValues(requestsWaitingForPassword,
			m.pubKey, (wtr) => wtr.reject(new window.Error(
				"Input tab was closed for public key: " +
				m.pubKey ))); break;
} });

window.wrappedJSObject.tryPasswordTab =
	cloneInto(tryPasswordTab, window, { cloneFunctions: true });
