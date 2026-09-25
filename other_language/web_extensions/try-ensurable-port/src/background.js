console.log("BACKGROUND BEGIN");

// FOR DEBUG. REMOVE IT.
const APP_ID = "556b2913-820c-46e7-9f37-3e0d6a67e680"

browser.runtime.onMessage.addListener((m, s) => {
	console.log("background.js: receive message:", m, s);
	switch(m.method) {
		case "sendMessageToMe": return sendMessageToMe(s);
		case "connectToMe":
			return new Promise((rs, rj) => connectToMe(s, rs, rj));
		case "openOptionsInTab": return openOptionsInTab();
		case "portConnectionToBackground":
			return portConnectionToBackground();
		case "testEnsurablePort": return testEnsurablePort();
	}
});


async function
sendMessageToMe(s)
{
	return browser.tabs.sendMessage(s.tab.id, {
		method: "messageFromBackground" });
}

async function
connectToMe(s, rs, rj)
{
	console.log("background.js: connectToMe");
	const port = browser.tabs.connect(s.tab.id, { name: "port" } );
	port.onDisconnect.addListener(() => {
		console.log("background.js: disconnect:", port);
		console.log("background.js: disconnect: port.error:",
			port.error);
		if (port.error) rj(new Error(port.error)); else rs(); });
	port.onMessage.addListener(m => {
		console.log("background.js: port receive:", m);
		port.disconnect(); rs(); });
	console.log("background.js: port is ", port);
	console.log( "background.js: port.error is ", port.error );
	port.postMessage("Foo Bar");
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
	listener(p)
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

// FOR DEBUG. USE EnsurablePortList.
async function
testEnsurablePort()
{
	console.log("background.js: testEnsurablePort");
	browser.runtime.onConnect.addListener(listener);

	function
	listener(p)
	{
		console.log("background.js: testEnsurablePort: listener:", p);
		p.onMessage.addListener(m => {
			console.log("background.js: testEnsurablePort:", m);
			switch(m.method) {
				case APP_ID + ":test-port:disconnect-ack":
					console.log("background: ACK");
					p.disconnect();
					break;
				default:
					p.postMessage({ method: APP_ID + ":test-port:disconnect" });
					browser.runtime.onConnect.removeListener(listener);
			}
		});
	}
}
