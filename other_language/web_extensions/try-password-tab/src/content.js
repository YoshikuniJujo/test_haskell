browser.runtime.sendMessage({ method: "contentStarted" });

const requestsWaitingForPassword = new Map();
const somethingResolvers = new Map();

const tryPasswordTab = {

	getSomething(pk, prm)
	{
		const rid = crypto.randomUUID();
		return new window.Promise(async (rslv, rjct) => {
			try {	browser.runtime.sendMessage({
					method: "queryPass", answer: pk });
				await new Promise((rs, rj) => { addToArrayMap(
					requestsWaitingForPassword, pk,
					{ resolve: rs, reject: rj } ); });
				somethingResolvers.set(rid, rslv);
				browser.runtime.sendMessage( {
					method: "getSomething", answer: pk,
					request: rid, parameter: prm } ); }
			catch(e) { rjct(e); } });
	}

};

browser.runtime.onMessage.addListener((m) => { switch (m.method) {
	case "passwordReady":
		forEachValues(requestsWaitingForPassword,
			m.publicKey, (wtr) => wtr.resolve()); break;
	case "inputTabClosed":
		forEachValues(requestsWaitingForPassword,
			m.publicKey, (wtr) => wtr.reject(new window.Error(
				"Input tab was closed for public key: " +
				m.publicKey ))); break;
	case "something": {
		const rs = somethingResolvers.get(m.request);
		somethingResolvers.delete(m.request);
		if (!rs) {
			console.error(
				"invalid input request",
				{ request: m.request } );
			throw new Error("Invalid input request"); }
		rs(m.value); break; } } });

window.wrappedJSObject.tryPasswordTab =
	cloneInto(tryPasswordTab, window, { cloneFunctions: true });

function
addToArrayMap(map, k, v)
{
	const vs = map.get(k); if (vs) vs.push(v); else map.set(k, [v]);
}

function
forEachValues(map, k, f)
{
	const vs = map.get(k); map.delete(k); for (const v of vs) f(v);
}
