import * as Bech32 from "./codec/bech32.js";
import * as KeyPair from "../generated/keyPair.js";
import * as Schnorr from "./crypto/schnorr.js";

const nsec = Bech32.decode(KeyPair.nsec);
const npub = Bech32.decode(KeyPair.npub);

console.log("try-sign");
console.log(npub);

document.documentElement.style.border = "5px solid green";

const nostr = {

	getPublicKey() {
		return new window.Promise(async (resolve) => {
			const result = Array.from(Bech32.decode(KeyPair.npub), b => b.toString(16).padStart(2, "0")).join("");
			resolve(result);
		});
	},

	signEvent(event) {
		console.log(event);
		console.log(event.created_at);
		console.log(event.kind);
		console.log(event.tags);
		console.log(event.content);

		return new window.Promise(async (resolve) => {

			const ev = {
				created_at: event.created_at,
				kind: event.kind,
				tags: event.tags,
				content: event.content };

			const rtn = await Schnorr.signEvent(ev, nsec, npub);

			console.log("rtn obtained");
			console.log(rtn.id);
			console.log("id obtained");

			const rslt = {
				created_at: rtn.created_at,
				kind: rtn.kind,
				tags: rtn.tags,
				content: rtn.content,
				id: rtn.id,
				pubkey: rtn.pubkey,
				sig: rtn.sig }
			resolve(cloneInto(rslt, window));
		});
	}
}

window.wrappedJSObject.nostr =
	cloneInto(nostr, window, { cloneFunctions: true });
