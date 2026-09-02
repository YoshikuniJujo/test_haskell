import * as Bech32 from "./codec/bech32.js";
import * as KeyPair from "../generated/keyPair.js";
import * as Schnorr from "./crypto/schnorr.js";

const nsec = Bech32.decode(KeyPair.nsec);
const npub = Bech32.decode(KeyPair.npub);

document.documentElement.style.border = "5px solid green";

const nostr = {

	getPublicKey() {
		return new window.Promise(async (resolve) => {
			const result = Array.from(Bech32.decode(KeyPair.npub), b => b.toString(16).padStart(2, "0")).join("");
			resolve(result);
		});
	},

	signEvent(event) {
		return new window.Promise(async (resolve) => {
			const rtn = await Schnorr.signEvent(event, nsec, npub);
			resolve(cloneInto(rtn, window));
		});
	}
}

window.wrappedJSObject.nostr =
	cloneInto(nostr, window, { cloneFunctions: true });
