document.body.style.border = "5px solid red";

const nostr = {

	getPublicKey() {
		return new window.Promise((resolve) => {
				resolve(
					"cc77438831fb0816e9f41443cf2603c90cdd7d558e49edc5f6b72326c22452d0"
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
