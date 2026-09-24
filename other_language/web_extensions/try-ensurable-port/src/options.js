console.log("OPTIONS BEGIN");

let openType; let pageId;

if (location.search === "") { openType = "browser"; pageId = "browser"; }
else {
	const params = new URLSearchParams(location.search);
	openType = params.get("openType"); pageId = params.get("pageId"); }

const messageFromBackground =
	document.querySelector("#message-from-background");

messageFromBackground.addEventListener("click", async () => {
	try {
		browser.runtime.onMessage.addListener(listener);
		await browser.runtime.sendMessage({ method: "sendMessageToMe" }); }
	finally {
		console.log("messageFromBackground: remove listener");
		browser.runtime.onMessage.removeListener(listener);
	}

	function
	listener(m, s)
	{
		console.log("options.js: recieve message:", m, s);
	}
});

const portConnectionFromBackground =
	document.querySelector("#port-connection-from-background");

portConnectionFromBackground.addEventListener("click", () => {

	browser.runtime.onConnect.addListener(port => {
		console.log("options.js: receive port", port.name);
		port.onMessage.addListener(m => {
			console.log("options.js: received:", m);
		});
	});

	browser.runtime.sendMessage( { method: "hello" } );

});


const openInTab = document.querySelector("#open-in-tab");

if (openType === "browser") openInTab.hidden = false;

openInTab.addEventListener("click", () => {
	browser.runtime.sendMessage({ method: "openOptionsInTab" });
});
