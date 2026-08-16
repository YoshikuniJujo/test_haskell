import { npub } from '../generated/sampleKeyPair.js'

document.body.style.border = "5px solid red";

const nostr = {

	getPublicKey() {
		return new window.Promise(async (resolve) => {

			const result = await browser.runtime.sendMessage({
				method: "getPublicKey"
			});

			console.log(result);

			resolve(
				result
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
