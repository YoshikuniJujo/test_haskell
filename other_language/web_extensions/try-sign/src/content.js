import * as S from "./crypto/schnorr.js";
import * as Bech32 from "./codec/bech32.js";
import * as KeyPair from "../generated/keyPair.js";

const { dp: sk } = Bech32.decode(KeyPair.nsec);
const { dp: pk } = Bech32.decode(KeyPair.npub);

const nostr = {

	getPublicKey()
	{
		return new window.Promise(rs =>
			rs(Array.from(pk, b =>
				b.toString(16).padStart(2, "0")).join("")));
	},

	signEvent(ev)
	{
		return new window.Promise(async rs =>
			rs(cloneInto(await S.signEvent(ev, sk, pk), window)));
	}

}

window.wrappedJSObject.nostr =
	cloneInto(nostr, window, { cloneFunctions: true });
