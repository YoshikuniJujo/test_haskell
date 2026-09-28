import { EnsurablePort } from "./ensurablePort.js";

console.log("OPTIONS BEGIN");

// FOR DEBUG. REMOVE IT.
const APP_ID = "556b2913-820c-46e7-9f37-3e0d6a67e680"

let openType; let pageId;

if (location.search === "") { openType = "browser"; pageId = "browser"; }
else {
	const params = new URLSearchParams(location.search);
	openType = params.get("openType"); pageId = params.get("pageId"); }

const messageFromBackground =
	document.querySelector("#message-from-background");

if (!messageFromBackground) throw(new Error("no #message-from-background"));

messageFromBackground.addEventListener("click", async () => {
	browser.runtime.onMessage.addListener(listener);
	try {	await browser.runtime.sendMessage({
			method: "sendMessageToMe" }); }
	finally {
		console.log("messageFromBackground: remove listener");
		browser.runtime.onMessage.removeListener(listener); }

	function
	listener(m: string, s: browser.runtime.MessageSender)
	{
		console.log("options.js: recieve message:", m, s);
	}
});

const portConnectionFromBackground =
	document.querySelector("#port-connection-from-background");

if (!portConnectionFromBackground) throw new Error("no #port-connection-from-background");

portConnectionFromBackground.addEventListener("click", async () => {
	browser.runtime.onConnect.addListener(listener);
	try {	await browser.runtime.sendMessage( {
		method: "connectToMe" } ); }
	finally {
		console.log("portConnectionFromBackground: remove listener");
		browser.runtime.onConnect.removeListener(listener); }

	function
	listener(p: browser.runtime.Port)
	{
		console.log("options.js: receive port:", p.name);
		p.onMessage.addListener(m => {
			console.log("options.js: received:", m);
			p.postMessage("Bar Baz"); });
	}
});

const portConnectionToBackground =
	document.querySelector("#port-connection-to-background");

if (!portConnectionToBackground) throw new Error("no #port-connection-to-background");

portConnectionToBackground.addEventListener("click", async () => {
	await browser.runtime.sendMessage({
		method: "portConnectionToBackground" });
	const port = browser.runtime.connect({ name: "port" });
	console.log("options.js:", port);
	port.onMessage.addListener(m => {
		console.log("options.js: receive:", m);
		port.postMessage({ method: "foobarbaz", content: "FOOBARBAZ" }); });
	port.onDisconnect.addListener(() => {
		console.log("options.js: disconnect"); }); });

const testEnsurablePort = document.querySelector("#test-ensurable-port");

if (!testEnsurablePort) throw new Error("no #test-ensurable-port");

testEnsurablePort.addEventListener("click", async () => {
	const eport = new EnsurablePort("test-port");
	await browser.runtime.sendMessage({
		method: "testEnsurablePort" });
	console.log("options: testEnsurablePort listener: after sendMessage");
	const p = eport.ensure();
	if (!p) throw new Error("no port");
	p.postMessage({ method: "", content: "ENSURABLE PORT TEST FROM options.js" });
});

const testEnsurablePortList = document.querySelector("#test-ensurable-port-list");

if (!testEnsurablePortList) throw new Error("no #test-ensurable-port-list");

testEnsurablePortList.addEventListener("click", async () => {
	browser.runtime.onConnect.addListener(listener);
	await browser.runtime.sendMessage({
		method: "testEnsurablePortList" });

	function
	listener(p: browser.runtime.Port)
	{
		console.log("options.js: port =", p);
		p.onMessage.addListener(m => {
			console.log("options.js:", m);
			if (hasMethod(m))
			switch(m.method) {
				case `${p.name}:disconnect-ack`:
					console.log("options.ts: DISCONNECT ACK");
					p.disconnect();
					browser.runtime.onConnect.removeListener(listener);
					break;
				default:
					console.log("options.js: HERE:", p.name, m);
					p.postMessage({ method: `${p.name}:disconnect` });
					break;
			}
		});
	}

});

const openInTab = document.querySelector<HTMLElement>("#open-in-tab");
if (!openInTab) throw new Error("no #open-in-tab");
if (openType === "browser") openInTab.hidden = false;
openInTab.addEventListener("click", () => {
	browser.runtime.sendMessage({ method: "openOptionsInTab" }); });

type MessageWithMethod = {
	method: string;
};

function hasMethod(m: object): m is MessageWithMethod {
	return "method" in m && typeof m.method === "string";
}
