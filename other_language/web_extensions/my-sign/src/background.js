import { InputTabs } from "./inputTabs.js";
import { EncryptedSecretKey } from "./crypto/ncryptsec.js";
import * as DB from "./db.js";
import * as Log from "./log.js";

import * as Bech32 from "./codec/bech32.js";

import { addToArrayMap, forEachValues } from "./mapArray.js"

console.log("background.js");
Log.write("BACKGROUND BEGIN");

const itbs = new InputTabs("input");
const otbs = new InputTabs("options");

(async () => {
	const ots = Object.values(await otbs.keyInputTabs());
	Log.write(`Options tabs: ${ots}`);
	const otkits = await otbs.keyInputTabs();
	Log.write(`Options key-tabs: ${JSON.stringify(otkits)}`);

})();

browser.runtime.onMessage.addListener( async (m, s) => {
	if (typeof m.class === "undefined") return globalMethod(m, s);
	else if (typeof m.instance === "undefined")
		throw Error("It need a instance if a class is defined.");
	else switch (m.class) {
		default:
			console.log("no such class: ", m, s);
	}
});

async function
globalMethod(m, s)
{
	console.log("message received", m);
	switch (m.method) {
		case "accountDisplayInfo": return getAccountMethod(s.url);
		case "publicKey":
			return hex(await getPublicKey(s.url, s.tab.id));
		case "prepareSymmetricKey": return qPswd(m.pubKey, s.tab.id);
		case "registerSymmetricKey":
			return rgSymkey(s.tab.id, m.pubKey, m.pswd);
		case "signEvent": return signEvent(m.pubKey, m.event);
		case "contentStarted": return pgVanished(s.tab.id);
		case "clientChanged": return broadcast({ method: "clientChanged" });
		case "openSettings": {
			console.log("background: openSettings");
			console.log(s.url);
			const cl = await getClient(s.url);
			let ot;
			let use;
			if (await DB.getUseClientSet()) {
				ot = await browser.tabs.create({
					active: false,
					url: browser.runtime.getURL(
						"options.html?openType=set&id=set")
				});
				use = await otbs.assign("set", s.tab.id, ot.id)
			}
			else {	ot = await browser.tabs.create({
					active: false,
					url: browser.runtime.getURL(
						"options.html?openType=uuid&id=" + encodeURIComponent(cl.uuid) )
				});
				use = await otbs.assign(cl.uuid, s.tab.id, ot.id)
			}
			console.log("openSettings:", use);
			if (use !== ot.id) {
				await browser.tabs.remove(ot.id);
			}
			await browser.tabs.update(use, { active: true });
			return;
		}
		case "testOptionsSender":
			console.log("options sender:", s);
			return;
		case "openOptionsInTab":
			console.log("background: openOptionsInTab");
			const ot2 = await browser.tabs.create({
				active: false,
				url: browser.runtime.getURL(
					"options.html?openType=tab&id=tab")
			});
			return;
		case "optionsStarted":
			console.log("background: optionsStarted");
			const use = await otbs.assign(m.id, null, s.tab.id);
			if (use !== s.tab.id) {
				console.log("remove not used");
				await browser.tabs.remove(s.tab.id);
			}
			await browser.tabs.update(use, { active: true });
			return;
		case "prepareClient":
			console.log("background: prepareClient");
			const c = await getClient(m.clientUrl);
			console.log("background: getClient return: ", c);
			if (c !== null) await browser.tabs.sendMessage(s.tab.id, { method: "clientReady", clientUrl: m.clientUrl });
			else {
				console.log("c is:", c);
				const ot = await browser.tabs.create({
					active: false,
					url: browser.runtime.getURL(
						"options.html?openType=url&id=" + encodeURIComponent(m.clientUrl) )
				});
				console.log(ot);
				console.log("getPublicKey", m.clientUrl, s.tab.id, ot.id)
				const use = await otbs.assign(m.clientUrl, s.tab.id, ot.id);
				if (use != ot.id) await browser.tabs.remove(ot.id);
				await browser.tabs.update(use, { active: true }); }

			return;
		case "clientSubmited":
			console.log("background: clientSubmited");
			const clnt = await getClient(m.id);
			console.log("background: clientSubmited:", clnt);
			if (clnt !== null) {
				const sts = await otbs.complete(m.id, s.tab.id);
				for (const st of sts)
					await browser.tabs.sendMessage(st, { method: "clientReady", clientUrl: m.id });
				await browser.tabs.update(sts[0], { active: true });
				await browser.tabs.remove(s.tab.id);
				return true; }
			else return false;
	}
}

browser.tabs.onRemoved.addListener(pgVanished);

const waitForClientChanged = new Map();

async function
getAccountMethod(url)
{
			console.log("getAccountMethod: ", url);
			const c = await getClient(url);
			if (c === null || c.displayAccount === false) return null;
			const pbk = c.publicKey;

			const acc = await getAccount(pbk);
			console.log("getAccountMethod: acc = ", acc);
			return {
				name: acc.name,
				publicKey: Bech32.encode("npub", acc.publicKey),
				positionX: c.positionX ?? 100,
				positionY: c.positionY ?? 0,
				backgroundColor: hexToRgb(c.backgroundColor ?? "#008000"),
				backgroundOpacity: c.backgroundOpacity ?? 0.5
			};
}

async function
rgSymkey(tid, pbk, pswd)
{
			const acc2 = await getAccount(unhex(pbk));
			if (await acc2.checkPassword(pswd)) {
				const { pswds = {} } =
					await browser.storage.session.get("pswds");
				pswds[pbk] = await acc2.getSymmetricKey(pswd);
				await browser.storage.session.set({ pswds });

				const sts = await itbs.complete(pbk, tid);
				for (const st of sts)
					await browser.tabs.sendMessage(st, { method: "pswdReady", pubKey: pbk });
				await browser.tabs.update(sts[0], { active: true });
//				console.log("TAB REMOVE 0", tid);
				await browser.tabs.remove(tid);
				return true; }
			else {	return false; }
}

async function
signEvent(pbk, evt)
{
			const acc = await getAccount(unhex(pbk));
			const { pswds = {} } = await browser.storage.session.get("pswds");
			const smk = pswds[pbk];
			return acc.signEvent(evt, smk);
}

const requestsWAitingForClient = new Map();

async function
getPublicKey(url, tid)
{
	const pbk = await getAccountPublicKey();
	if (pbk !== null) return pbk;

	console.log("getPublicKey: ", url);

	const { promise: pr, resolve: rs, reject: rj } = Promise.withResolvers();

	console.log("after Promise.withResolvers");

//	waitForClientChanged.set(url, { resolve: rs, reject: rj });
	addToArrayMap(waitForClientChanged, url, { resolve: rs, reject: rj });

	console.log(waitForClientChanged);

	const ot = await browser.tabs.create({
		active: false,
		url: browser.runtime.getURL(
			"options.html?openType=url&id=" + encodeURIComponent(url) )
	});
	console.log(ot);
	console.log("getPublicKey", url, tid, ot.id)
	const use = await otbs.assign(url, tid, ot.id);
	if (use != ot.id) await browser.tabs.remove(ot.id);
	await browser.tabs.update(use, { active: true });

	throw new Error("No client matches sender URL: " + url);

	async function
	getAccountPublicKey()
	{
		const c = await getClient(url);
		if (c === null) return null;
		else return c.publicKey;
	}
}

async function
getClient(url)
{
	console.log("getClient begin", url);
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
			"input.html?" +
			"accountName=" + encodeURIComponent((await getAccount(unhex(pk))).name) +
			"&publicKey=" + encodeURIComponent(pk) ) });
	const use = await itbs.assign(pk, st, it.id);
	if (use != it.id) {
//		console.log("TAB REMOVE 2", tid);
		await browser.tabs.remove(it.id);
	}
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

function
hexToRgb(hex)
{
	console.log("hexToRgb", hex);
	return {
		red: parseInt(hex.slice(1, 3), 16),
		green: parseInt(hex.slice(3, 5), 16),
		blue: parseInt(hex.slice(5, 7), 16)
	};
}

async function
pgVanished(vt)
{
	console.log("vanished:", vt);
	const r = await itbs.tabClosed(vt);
	for (const c of r.toClose) {
		console.log("TAB REMOVE 3", c);
		await browser.tabs.remove(c);
	}
	for (const c of r.cancelled) for (const s of c.sources)
		await browser.tabs.sendMessage(
			s, { method: "inputPageVanished", pubKey: c.pubKey });

	console.log("pgVanished:", r.cancelled[0]);
	browser.tabs.update(r.cancelled[0]?.sources[0], { active: true });

	console.log("otbs");
	const s = await otbs.tabClosed(vt);
	console.log("*** pgVanished:", s);
	console.log("*** pgVanished:", s.toClose);
	console.log("*** pgVanished:", s.cancelled);
	for (const d of s.toClose) {
		console.log("TAB REMOVE 4", d);
		await browser.tabs.remove(d);
	}
	console.log("HERE");
	for (const d of s.cancelled) for (const s of d.sources) {
		console.log(s);
		await browser.tabs.sendMessage(
			s, { method: "optionPageVanished", isUrl: isUrl(d.pubKey), clientUrl: d.pubKey });
	}

	browser.tabs.update(s.cancelled[0]?.sources[0], { active: true });
}

function
isUrl(s)
{
	try {
		new URL(s);
		return true;
	} catch {
		return false;
	}
}
