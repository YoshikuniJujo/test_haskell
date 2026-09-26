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

const portConnectionToBackground =
	document.querySelector("#port-connection-to-background");
portConnectionToBackground.addEventListener("click", async () => {
	await browser.runtime.sendMessage({
		method: "portConnectionToBackground" });
	const port = browser.runtime.connect({ name: "port" });
	console.log("options.js:", port);
	port.onMessage.addListener(m => {
		console.log("options.js: receive:", m);
		port.postMessage("FOOBARBAZ"); });
	port.onDisconnect.addListener(() => {
		console.log("options.js: disconnect"); }); });

const eport = new EnsurablePort("test-port");

const testEnsurablePort = document.querySelector("#test-ensurable-port");
testEnsurablePort.addEventListener("click", async () => {
	await browser.runtime.sendMessage({
		method: "testEnsurablePort" });
	console.log("options: testEnsurablePort listener: after sendMessage");
	const p = eport.ensure();
	p.postMessage("ENSURABLE PORT TEST FROM options.js");
});

const testEnsurablePortList = document.querySelector("#test-ensurable-port-list");
testEnsurablePortList.addEventListener("click", async () => {
	await browser.runtime.onConnect.addListener(p => {
		console.log("options.js: port =", p);
		p.onMessage.addListener(m => {
			console.log("options.js:", m);
			switch(m.method) {
				default:
					console.log("options.js: HERE:", p.name, m);
					p.postMessage({ method: `${p.name}:disconnect` });
					break;
			}
		});
	});
	await browser.runtime.sendMessage({
		method: "testEnsurablePortList" });
});

const openInTab = document.querySelector("#open-in-tab");
if (openType === "browser") openInTab.hidden = false;
openInTab.addEventListener("click", () => {
	browser.runtime.sendMessage({ method: "openOptionsInTab" }); });
