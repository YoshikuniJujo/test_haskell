browser.runtime.sendMessage({ method: "contentStarted" });

const requestsWaitingForAnswer = new Map();
const somethingResolvers = new Map();

const tryPasswordTab = {

	getSomething(aid, prm)
	{
		const rid = crypto.randomUUID();
		return new window.Promise(async (rslv, rjct) => {
			try {	browser.runtime.sendMessage({
					method: "queryPass", answer: aid });
				await new Promise((rs, rj) => { addToArrayMap(
					requestsWaitingForAnswer, aid,
					{ resolve: rs, reject: rj } ); });
				somethingResolvers.set(rid, rslv);
				browser.runtime.sendMessage( {
					method: "getSomething", answer: aid,
					request: rid, parameter: prm } ); }
			catch(e) { rjct(e); } });
	}

};

browser.runtime.onMessage.addListener((m) => { switch (m.method) {
	case "passwordReady":
		forEachValues(requestsWaitingForAnswer,
			m.answer, (wtr) => wtr.resolve()); break;
	case "passError":
		forEachValues(requestsWaitingForAnswer,
			m.answer, (wtr) => wtr.reject(new window.Error(
				"Input tab was closed for answer: " +
				m.answer ))); break;
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
