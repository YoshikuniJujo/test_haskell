import { addToArrayMap, forEachValues } from "./mapArray.js"

document.documentElement.style.border = "5px solid green";
browser.runtime.sendMessage({ method: "contentStarted" });

const requestsWaitingForPassword = new Map();

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

(async () => {
	const client = await browser.runtime.sendMessage({ method: "get-account" });
	div.textContent = client;
})();

document.body.append(div);

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
				rs(ev);
			}
			catch (e) { rj(cloneInto(e, window)); }
		});
	}
}

browser.runtime.onMessage.addListener((m) => { switch (m.method) {
	case "pswdReady":
		forEachValues(requestsWaitingForPassword,
			m.pubKey, wtr => wtr.resolve()); break;
} });

window.wrappedJSObject.nostr =
	cloneInto(nostr, window, { cloneFunctions: true });
