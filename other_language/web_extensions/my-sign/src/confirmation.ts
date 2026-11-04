console.log("confirmation.ts: BEGIN");

const params = new URLSearchParams(location.search);
console.log("confirmation.ts: params:", params);

const messageElement = document.querySelector("#message");

if (messageElement !== null)
	messageElement.textContent = params.get("message");

const msg = {
	class: params.get("class"),
	method: params.get("method"),
	tabId: params.get("tabId"),
	clientUuid: params.get("clientUuid")
};

browser.runtime.sendMessage(msg);
