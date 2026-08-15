import { npub } from '../generated/sampleKeyPair.js'

document.body.style.border = "5px solid red";

const nostr = {

	getPublicKey() {
		return new window.Promise((resolve) => {
			resolve(
				npub
			);
		});
	},

	async signEvent(event) {
		throw new Error("yet");
	}
};

window.wrappedJSObject.nostr =
	cloneInto(nostr, window, {
		cloneFunctions: true
	});
