import { InputTabs } from "./inputTabs.js";
import * as DB from "./db.js"

console.log("background.js");

const itbs = new InputTabs();

browser.runtime.onMessage.addListener( async (m, s) => {
	console.log("message received", m);
	switch (m.method) {
		case "get-account":
			console.log(s.url);
			return "dummy account";
		case "get-public-key":
			return hex(await getPublicKey(s));
		case "queryPswd":
			return qPswd(m.pubKey, s.tab.id);
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

async function
qPswd(pk, st)
{
	const { pswds = {} } = await browser.storage.session.get("pswds");
	if (pswds[pk] !== undefined) {
		await browser.tabs.sendMessage(
			st, { method: "pswdReady", pubKey: pk }); return; }
	const it = await browser.tabs.create({
		active: false,
		url: browser.runtime.getURL(
			`input.html?publicKey=${encodeURIComponent(pk)}` ) });
	const use = await itbs.assign(pk, st, it.id);
	if (use != it.id) await browser.tabs.remove(it.id);
	await browser.tabs.update(use, { active: true });
	await browser.tabs.sendMessage(st, { method: "pswdReady", pubKey: pk });
}

function
hex(bs)
{
	return [...bs]
		.map(b => b.toString(16).padStart(2, "0")).join("");
}
