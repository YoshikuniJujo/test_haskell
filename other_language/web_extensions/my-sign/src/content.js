import { addToArrayMap, forEachValues } from "./mapArray.js"

document.documentElement.style.border = "5px solid green";

const div = document.createElement("div");

let acount = null;

(async () => {
	await browser.runtime.sendMessage({ method: "contentStarted" });

	Object.assign(div.style, {
		position: "fixed",
		padding: "8px 12px",
		background: "rgba(0, 128, 0, 0.5)",
		color: "white",
		whiteSpace: "nowrap",
		zIndex: "2147483647"
	});

	document.body.append(div);
	account = await browser.runtime.sendMessage({ method: "get-account" });

	if (account !== null) {
		div.textContent = account.name + " " + account.publicKey.slice(0, 15) + "...";

		setAccountPosition(account);
	} else {
		div.hidden = true; }
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

browser.runtime.onMessage.addListener(async (m) => { switch (m.method) {
	case "pswdReady":
		console.log("content: pswdReady");
		console.log(m.pubKey);
		console.log(requestsWaitingForPassword);
		forEachValues(requestsWaitingForPassword,
			m.pubKey, wtr => wtr.resolve()); break;
	case "clientChanged":
		console.log("content: clientChanged");
		account = await browser.runtime.sendMessage({ method: "get-account" });
		console.log(account.positionX, account.positionY);
		if (account === null) {
			div.hidden = true;
			break; }
		div.hidden = false;
		div.textContent = account.name + " " + account.publicKey.slice(0, 15) + "...";

		setAccountPosition(account);

		break;
} });

window.wrappedJSObject.nostr =
	cloneInto(nostr, window, { cloneFunctions: true });

function
setAccountPosition(account)
{
		const rect = div.getBoundingClientRect();
		const left =
			(innerWidth - rect.width) * account.positionX / 100;
		const top =
			(innerHeight - rect.height) * account.positionY /100;
		div.style.left = `${left}px`
		div.style.top = `${top}px`
}

window.addEventListener("resize", () => {
	if (!div.hidden) setAccountPosition(account);
});
