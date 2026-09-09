import * as DB from "./db.js"

console.log("background.js");

browser.runtime.onMessage.addListener( async (m, s) => {
	console.log("message received", m);
	switch (m.method) {
		case "get-account":
			console.log(s.url);
			return "dummy account";
		case "get-public-key":
			return hex(await getPublicKey(s));
		case "queryPswd":
			console.log("background: queryPswd");
			await browser.tabs.sendMessage(
				s.tab.id, { method: "pswdReady", pubKey: m.pubKey });
			return;
		case "contentStarted":
			console.log("background: contentStarted");
			return ;
	}
});

async function
getPublicKey(s)
{
	const cs = await DB.getClients();
	for (const c of cs) {
		const pattern = new URLPattern(c.urlPattern);
		if (pattern.test(s.url)) return c.publicKey; }
	throw new Error("No client matches sender URL: " + s.url);
}

function
hex(bs)
{
	return [...bs]
		.map(b => b.toString(16).padStart(2, "0")).join("");
}
