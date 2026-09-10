import { InputTabs } from "./inputTabs.js";
import { EncryptedSecretKey } from "./crypto/ncryptsec.js";
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
		case "returnPswd":
			console.log("background: returnPswd")
			console.log(m.pubKey);
			console.log(unhex(m.pubKey));
			const acc = await DB.getAccount(unhex(m.pubKey));

			// DEBUG ONLY. DELETE IT.
			console.log(acc);
			console.log(
				acc.publicKey,
				acc.name,
				acc.version,
				acc.logN,
				acc.salt,
				acc.nonce,
				acc.keySecurityByte,
				acc.ciphertext,
				acc.saltForCheckPassword,
				acc.hashForCheckPassword )

			const acc2 = new EncryptedSecretKey(
				acc.publicKey,
				acc.name,
				acc.logN,
				acc.salt,
				acc.nonce,
				acc.keySecurityByte,
				acc.ciphertext,
				acc.saltForCheckPassword,
				acc.hashForCheckPassword )

			console.log(acc2);
			if (await acc2.checkPassword(m.pswd)) {
				const { pswds = {} } =
					await browser.storage.session.get("pswds");
				pswds[m.pubKey] = m.pswd;
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
