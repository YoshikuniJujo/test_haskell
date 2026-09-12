import { addToArrayMap, forEachValues } from "./mapArray.js"

document.documentElement.style.border = "5px solid green";

(async () => {
	await browser.runtime.sendMessage({ method: "contentStarted" });

	const account = await browser.runtime.sendMessage({ method: "get-account" });

	const div = document.createElement("div");
	Object.assign(div.style, {
		position: "fixed",
		top: "10px",
		right: "10px",
		padding: "8px 12px",
		background: "rgba(0, 128, 0, 0.5)",
		color: "white",
		zIndex: "2147483647"
	});
	div.textContent = account;
	document.body.append(div);
})();


const requestsWaitingForPassword = new Map();

const nostr = {

	getPublicKey()
	{
		return new window.Promise(async (rs, rj) => {
			try {
				const v = await browser.runtime.sendMessage({
					method: "get-public-key"
				});
				rs(v);
			}
			catch (e) { rj(cloneInto(e, window)); }
		});
	},

	signEvent(ev)
	{
		return new window.Promise(async (rs, rj) => {
			try {
				const pbk = await browser.runtime.sendMessage({
					method: "get-public-key"
				});
				await new Promise((rs, rj) => {
					addToArrayMap(
						requestsWaitingForPassword, pbk,
						{ resolve: rs, reject: rj } );
					console.log("signEvent: ", requestsWaitingForPassword);
					browser.runtime.sendMessage({
						method: "queryPswd", pubKey: pbk });
				});
				const sig = await browser.runtime.sendMessage({
					method: "sign-event",
					pubKey: pbk,
					event: ev
				});
				rs(cloneInto(sig, window));
			}
			catch (e) { rj(cloneInto(e, window)); }
		});
	}
}

browser.runtime.onMessage.addListener((m) => { switch (m.method) {
	case "pswdReady":
		console.log("content: pswdReady");
		console.log(m.pubKey);
		console.log(requestsWaitingForPassword);
		forEachValues(requestsWaitingForPassword,
			m.pubKey, wtr => wtr.resolve()); break;
} });

window.wrappedJSObject.nostr =
	cloneInto(nostr, window, { cloneFunctions: true });
