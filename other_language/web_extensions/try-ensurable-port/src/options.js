console.log("OPTIONS BEGIN");

let openType; let pageId;

if (location.search === "") { openType = "browser"; pageId = "browser"; }
else {
	const params = new URLSearchParams(location.search);
	openType = params.get("openType"); pageId = params.get("pageId"); }

const messageFromBackground =
	document.querySelector("#message-from-background");

messageFromBackground.addEventListener("click", async () => {
	browser.runtime.onMessage.addListener(listener);
	try {	await browser.runtime.sendMessage({
			method: "sendMessageToMe" }); }
	finally {
		console.log("messageFromBackground: remove listener");
		browser.runtime.onMessage.removeListener(listener); }

	function
	listener(m, s)
	{
		console.log("options.js: recieve message:", m, s);
	}
});

const portConnectionFromBackground =
	document.querySelector("#port-connection-from-background");

portConnectionFromBackground.addEventListener("click", async () => {
	browser.runtime.onConnect.addListener(listener);
	try {	await browser.runtime.sendMessage( {
		method: "connectToMe" } ); }
	finally {
		console.log("portConnectionFromBackground: remove listener");
		browser.runtime.onConnect.removeListener(listener); }

	function
	listener(p)
	{
		console.log("options.js: receive port:", p.name);
		p.onMessage.addListener(m => {
			console.log("options.js: received:", m);
			p.postMessage("Bar Baz"); });
	}
});


const openInTab = document.querySelector("#open-in-tab");
if (openType === "browser") openInTab.hidden = false;
openInTab.addEventListener("click", () => {
	browser.runtime.sendMessage({ method: "openOptionsInTab" }); });
