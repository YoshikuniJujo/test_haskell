import { EnsurablePortList } from "./ensurablePort.js"

console.log("BACKGROUND BEGIN");

// FOR DEBUG. REMOVE IT.
const APP_ID = "556b2913-820c-46e7-9f37-3e0d6a67e680"

type BackgroundMessage =
	| { method: "sendMessageToMe"; }
	| { method: "connectToMe"; }
	| { method: "openOptionsInTab"; }
	| { method: "portConnectionToBackground"; }
	| { method: "testEnsurablePort"; }
	| { method: "testEnsurablePortList", name: string; }


browser.runtime.onMessage.addListener((
	m: BackgroundMessage,
//	m,
	s : browser.runtime.MessageSender ) : Promise<void> | undefined => {
//	m,
//	s ) : Promise<void> => {
	console.log("background.js: receive message:", m, s);
	switch(m.method) {
		case "sendMessageToMe": return sendMessageToMe(s);
		case "connectToMe":
			return new Promise((rs, rj) => connectToMe(s, rs, rj));
		case "openOptionsInTab": return openOptionsInTab();
		case "portConnectionToBackground":
			return portConnectionToBackground();
		case "testEnsurablePort": return testEnsurablePort();
		case "testEnsurablePortList": return testEnsurablePortList(m.name, s);
	}
});

/**
 * @param {browser.runtime.MessageSender} s
 */

async function
sendMessageToMe(s : browser.runtime.MessageSender)
{
	if (!s.tab) throw new Error("sender is not from a tab");
	if (!s.tab.id) throw new Error("no s.tab.id");
	return browser.tabs.sendMessage(s.tab.id, {
		method: "messageFromBackground" });
}

/**
 * @param {browser.runtime.MessageSender} s
 */

function
connectToMe(
	s: browser.runtime.MessageSender,
	rs: () => void, rj: (e: Error) => void)
{
	if (!s.tab) throw new Error("sender is not from a tab");
	if (!s.tab.id) throw new Error("no s.tab.id");
	console.log("background.js: connectToMe");
	const port = browser.tabs.connect(s.tab.id, { name: "port" } );
	port.onDisconnect.addListener(() => {
		console.log("background.js: disconnect:", port);
		console.log("background.js: disconnect: port.error:",
			port.error);
		if (port.error) rj(new Error(port.error.message)); else rs(); });
	port.onMessage.addListener(m => {
		console.log("background.js: port receive:", m);
		port.disconnect(); rs(); });
	console.log("background.js: port is ", port);
	console.log( "background.js: port.error is ", port.error );
	port.postMessage({ method: "foobar", content: "Foo Bar" });
}

async function
openOptionsInTab()
{
	console.log("background: openInTab");
	await browser.tabs.create({
		active: true,
		url: browser.runtime.getURL(
			"options.html?openType=tab&pageId=tab" ) });
}

async function
portConnectionToBackground()
{
	browser.runtime.onConnect.addListener(listener);

	async function
	listener(p: browser.runtime.Port)
	{
		console.log("background.js", p);
		p.onMessage.addListener(m => {
			console.log("background.js: receive:", m);
			p.disconnect();
			browser.runtime.onConnect.removeListener(listener); });
		p.onDisconnect.addListener(() => {
			console.log("background.js: disconnect") });
		p.postMessage("HOGEPIYO");
	}
}

type TestEnsurablePortMessage =
	| { method: "TestEnsurablePortMessage"; }

// FOR DEBUG. USE EnsurablePortList.
async function
testEnsurablePort()
{
	console.log("background.js: testEnsurablePort");
	browser.runtime.onConnect.addListener(listener);

	function
	listener(p: browser.runtime.Port)
	{
		console.log("background.js: testEnsurablePort: listener:", p);
		p.onMessage.addListener((message: object) => {
			const m = message as TestEnsurablePortMessage;
			console.log("background.js: testEnsurablePort:", m);
			switch(m.method) {
				case APP_ID + ":test-port:disconnect-ack":
					console.log("background: ACK");
					p.disconnect();
					break;
				default:
					console.log("background: HERE:", m);
					p.postMessage({ method: APP_ID + ":test-port:disconnect" });
					browser.runtime.onConnect.removeListener(listener);
			}
		});
	}
}

async function
testEnsurablePortList(nm: string, s: browser.runtime.MessageSender)
{
	console.log("background.js: testEnsurablePortList:", s);
	const eport = new EnsurablePortList();
	if (!s.tab) throw new Error("sender is not from a tab");
	if (!s.tab.id) throw new Error("no s.tab.id");
	if (nm === "browser") {
		eport.post(nm, { method: "foobar", content: "ENSURABLE PORT LIST TEST from background.js" });
	}
	else {
		const p = eport.ensure(nm, s.tab.id);
		console.log("background.js: port =", p);
		p.postMessage({ method: "foobar", content: "ENSURABLE PORT LIST TEST from background.js" });
	}
}
