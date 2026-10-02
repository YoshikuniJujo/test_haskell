import { addToArrayMap, forEachValues } from "./mapArray.js"

console.log("content.ts begin");

document.documentElement.style.border = "5px solid green";

const div = document.createElement("div");

type AccountDisplayInfo = {
	name: string,
	publicKey: string,
	backgroundColor: Color,
	backgroundOpacity: number,
	positionX: number, positionY: number
}

type Color = { red: number, green: number, blue: number }

let account: AccountDisplayInfo | null = null;

console.log("content.ts: before (async () => {");

(async () => {
	console.log("content.ts: before contentStarted");
	await browser.runtime.sendMessage({ method: "contentStarted" });

	console.log("content.ts: after contentStarted");

	Object.assign(div.style, {
		position: "fixed",
		padding: "8px 12px",
		background: "rgba(0, 128, 0, 0.5)",
		color: "white",
		whiteSpace: "nowrap",
		zIndex: "2147483647"
	});

	console.log("content.ts: HERE");

	document.body.append(div);
	account = await browser.runtime.sendMessage({ method: "accountDisplayInfo" });
	console.log("*** content.ts: account =", account);

	if (account !== null) {
		div.textContent = account.name + " " + account.publicKey.slice(0, 15) + "...";

		setAccountPosition(account);
		console.log(account.backgroundColor);
		const [r, g, b] = [
			account.backgroundColor.red,
			account.backgroundColor.green,
			account.backgroundColor.blue ]
		div.style.background = `rgb(${r} ${g} ${b}`;
		div.style.opacity = `${account.backgroundOpacity}`;
		console.log(account.backgroundOpacity);
	} else {
		div.hidden = true; }
	document.body.append(div);
})();

console.log("content.ts: after (async () => { ... })();");

const error = document.createElement("div");
error.popover = "manual";
error.textContent = "Password input was canceled";

Object.assign(error.style, {
	color: "white",
	background: "rgba(220, 0, 0, 0.5)",
	padding: "0.7em 1.2em",
	border: "none",
	borderRadius: "0.3em",
	pointerEvents: "none",
	position: "fixed"
});

document.body.prepend(error);

document.addEventListener("input", () => error.hidePopover());


const requestsWaitingForPassword = new Map();
const waitingForClient = new Map();

const nostr = {

	getPublicKey()
	{
		return new window.Promise(async (rs, rj) => {
			try {

				const url = location.href;
				console.log("content.js:", url);

				await new Promise((rs, rj) => {
					addToArrayMap(
						waitingForClient, url,
						{ resolve: rs, reject: rj } );
					browser.runtime.sendMessage({
						method: "prepareClient", clientUrl: url });
				});

				const v = await browser.runtime.sendMessage({
					method: "publicKey"
				});
				rs(v);
			}
			catch (e) { rj(cloneInto(e, window)); }
		});
	},

	signEvent(ev: object)
	{
		return new window.Promise(async (rs, rj) => {
			try {
				const pbk = await browser.runtime.sendMessage({
					method: "publicKey"
				});
				await new Promise((rs, rj) => {
					addToArrayMap(
						requestsWaitingForPassword, pbk,
						{ resolve: rs, reject: rj } );
					console.log("signEvent: ", requestsWaitingForPassword);
					browser.runtime.sendMessage({
						method: "prepareSymmetricKey", pubKey: pbk });
				});
				const sig = await browser.runtime.sendMessage({
					method: "signEvent",
					pubKey: pbk,
					event: ev
				});
				rs(cloneInto(sig, window));
			}
			catch (e) { setTimeout(() => { rj(cloneInto(e, window)) }, 0); }
		});
	}
}

div.addEventListener("click", () => {
	console.log("DIV CLICKED");
	browser.runtime.sendMessage({ method: "openSettings" });
});

type Message =
	| { method: "pswdReady", pubKey: string }
	| { method: "clientChanged" }
	| { method: "inputPageVanished", pubKey: string }
	| { method: "clientReady", clientUrl: string }
	| { method: "optionPageVanished", isUrl: boolean, clientUrl: string }

type RequestWaitingForPassword = { resolve: () => void, reject: (err: Error) => void }

browser.runtime.onMessage.addListener(async (m: Message) => { switch (m.method) {
	case "pswdReady":
		console.log("content: pswdReady");
		console.log(m.pubKey);
		console.log(requestsWaitingForPassword);
		forEachValues(requestsWaitingForPassword,
			m.pubKey, (wtr: RequestWaitingForPassword) => wtr.resolve()); break;
	case "clientChanged":
		console.log("content: clientChanged");
		account = await browser.runtime.sendMessage({ method: "accountDisplayInfo" });
		if (account === null) {
			div.hidden = true;
			break; }
		div.hidden = false;
		div.textContent = account.name + " " + account.publicKey.slice(0, 15) + "...";

		console.log("clientChanged", account);
		setAccountPosition(account);
		console.log(account.backgroundColor);
		const [r, g, b] = [
			account.backgroundColor.red,
			account.backgroundColor.green,
			account.backgroundColor.blue ]
		div.style.background = `rgb(${r} ${g} ${b} / ${account.backgroundOpacity})`;
		console.log(account.backgroundOpacity);

		break;
	case "inputPageVanished":
		console.log("content: inputPageVanished");
		forEachValues(requestsWaitingForPassword,
			m.pubKey, (wtr: RequestWaitingForPassword) => {
				error.showPopover();
				setErrorPosition();
				wtr.reject(new Error("input page vanished"))
			});
		break;
	case "clientReady":
		console.log("content: clientReady");
		forEachValues(waitingForClient,
			m.clientUrl, (wtr: RequestWaitingForPassword) => wtr.resolve()); break;
		break;
	case "optionPageVanished":
		console.log("content: clientPageVanished");
		console.log(m);
		if (m.isUrl)
			forEachValues(waitingForClient,
				m.clientUrl, (wtr: RequestWaitingForPassword) => wtr.reject(new Error ("options page vanished")));
		break;
} });

window.wrappedJSObject.nostr =
	cloneInto(nostr, window, { cloneFunctions: true });

function
setAccountPosition(account: AccountDisplayInfo)
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
	if (!account) throw new Error("bad1");
	if (!div.hidden) setAccountPosition(account);
	if (error.matches(":popover-open")) setErrorPosition();
});

function
setErrorPosition() {
	const ss = getComputedStyle(error);
	error.style.inset = "auto";
	error.style.margin = "0";
	const rect = error.getBoundingClientRect();
	error.style.left = `${(innerWidth - rect.width) / 2}px`;
	error.style.top = `${(innerHeight - rect.height) / 2}px`;
}
