import { npub } from "../generated/sampleKeyPair.js";

console.log("background started");

setInterval(() => {
	console.log("background is alive");
}, 5000);

browser.runtime.onMessage.addListener((message, sender) => {
	if (message?.method !== "getPublicKey") {
		return;
	}

	console.log("getPublicKey", sender.tab?.id, sender.tab?.url);

	return npub;
});
