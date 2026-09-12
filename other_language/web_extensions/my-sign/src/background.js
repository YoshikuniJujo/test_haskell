import { InputTabs } from "./inputTabs.js";
import { EncryptedSecretKey } from "./crypto/ncryptsec.js";
import * as DB from "./db.js"

import * as Bech32 from "./codec/bech32.js"

console.log("background.js");

const itbs = new InputTabs();

browser.runtime.onMessage.addListener( async (m, s) => {
	console.log("message received", m);
	switch (m.method) {
		case "get-account": {
			console.log(s.url);
			const c = await getClient(s.url);
			if (c === null || c.displayAccount === false) return null;
			const pbk = c.publicKey;

			const acc = await getAccount(pbk);
			return {
				name: acc.name,
				publicKey: Bech32.encode("npub", acc.publicKey),
				positionX: c.positionX ?? 100,
				positionY: c.positionY ?? 0
			}; }
		case "get-public-key":
			return hex(await getPublicKey(s));
		case "queryPswd":
			return qPswd(m.pubKey, s.tab.id);
		case "returnPswd":
			console.log("background: returnPswd")
			console.log(m.pubKey);
			console.log(unhex(m.pubKey));
			const acc2 = await getAccount(unhex(m.pubKey));
			console.log(acc2);
			if (await acc2.checkPassword(m.pswd)) {
				const { pswds = {} } =
					await browser.storage.session.get("pswds");
				pswds[m.pubKey] = await acc2.getSymmetricKey(m.pswd);
				await browser.storage.session.set({ pswds });

				const sts = await itbs.complete(m.pubKey, s.tab.id);
				for (const st of sts)
					await browser.tabs.sendMessage(st, { method: "pswdReady", pubKey: m.pubKey });
				await browser.tabs.update(sts[0], { active: true });
				await browser.tabs.remove(s.tab.id); }
			else {	await browser.tabs.sendMessage(s.tab.id, { method: "wrongPswd" }); }
			return;
		case "contentStarted":
			console.log("background: contentStarted");
			return ;
		case "sign-event":
			console.log("background: sign-event");
			const acc = await getAccount(unhex(m.pubKey));
			const { pswds = {} } = await browser.storage.session.get("pswds");
			const smk = pswds[m.pubKey];
			return acc.signEvent(m.event, smk);
		case "clientChanged":
			console.log("background: clientChanged");
			await broadcast({ method: "clientChanged" });
			return;
	}
});

async function
getPublicKey(s)
{
	const pbk = await getAccountPublicKey(s.url);
	if (pbk !== null) return pbk;
	browser.runtime.openOptionsPage();
	throw new Error("No client matches sender URL: " + s.url);

	async function
	getAccountPublicKey(url)
	{
		const c = await getClient(url);
		if (c === null) return null;
		else return c.publicKey;
	}
}

async function
getClient(url)
{
	console.log("getClient begin");
	const cs = await DB.getClients();
	cs.sort((a, b) => (b.priority ?? 100) - (a.priority ?? 100));
	for (const c of cs) {
		const pattern = new URLPattern(c.urlPattern);
		if (pattern.test(url)) return c; }
	return null;
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
}

function
hex(bs)
{
	return [...bs]
		.map(b => b.toString(16).padStart(2, "0")).join("");
}

function
unhex(s)
{
	const bs = new Uint8Array(s.length / 2);
	for (let i = 0; i < bs.length; ++i)
		bs[i] = parseInt(s.slice(i * 2, i * 2 + 2), 16);
	return bs;
}

async function
getAccount(pbk)
{
			const acc = await DB.getAccount(pbk);
			return new EncryptedSecretKey(
				acc.publicKey,
				acc.name,
				acc.logN,
				acc.salt,
				acc.nonce,
				acc.keySecurityByte,
				acc.ciphertext,
				acc.saltForCheckPassword,
				acc.hashForCheckPassword )
}

async function
broadcast(m)
{
	const tabs = await browser.tabs.query({});
	for (const tab of tabs) {
		if (tab.id === undefined) continue;
		try {
			await browser.tabs.sendMessage(tab.id, m);
		}
		catch (e) { console.log("broadcast", e) }
	}
}
