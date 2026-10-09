import { InputTabs } from "./inputTabs.js";
import { EncryptedSecretKey } from "./crypto/ncryptsec.js";
import * as DB from "./db.js";
import { Log } from "./log2.js";

import * as Bech32 from "./codec/bech32.js";

import { addToArrayMap, forEachValues } from "./mapArray.js";

import type { Client, ClientSummary, OptionsTabTag } from "./types.js";

import { isUrl } from "./tools.js";
import { optionsTabTag } from "./types.js";

type Event = {
	created_at: number,
	kind: number,
	tags: string[][],
	content: string
}

console.log("background.js");
Log.write("BACKGROUND BEGIN");

const itbs = new InputTabs("input");
const otbs = new InputTabs("options");

(async () => {
	const ots = [...(await otbs.keyInputTabs())]
		.filter(([key]) => key !== "browser")
		.map(([, value]) => value);
	Log.write(`Options tabs: ${ots}`);

	Log.setLogTabs(ots);

})();

browser.runtime.onMessage.addListener( (m, s) => {
	if (typeof m.class === "undefined") return globalMethod(m, s);
	else if (typeof m.instance === "undefined")
		throw Error("It need a instance if a class is defined.");
	else switch (m.class) {
		case "Options":
			console.log("background.ts: Options:", m);
			return optionsMethod(m, s);
		default:
			console.log("no such class: ", m, s);
	}
});

type OptionsMethod =
	| { class: "Options", method: "optionsBegin" }

async function
optionsMethod(m: OptionsMethod, s: browser.runtime.MessageSender)
{
	switch(m.method) {
		case "optionsBegin":
			console.log("background.ts: optionsMethod:", m, s);
			if (s.tab?.id === undefined) throw new Error("bad");
			const id = await otbs.key(s.tab.id);
			let tg;
			if (id === null) {
				const use = await otbs.assign("browser", null, s.tab.id);
				if (use !== s.tab.id)
					await browser.tabs.remove(s.tab.id);
				await browser.tabs.update(use, { active: true });
				console.log("background.ts: optionsMethod:",
					await otbs.keyInputTabs());
				return;
			} else {
				tg = optionsTabTag(id);
			}
			return;
	}
}

type GlobalMethod =
	| { method: "accountDisplayInfo"; }
	| { method: "publicKey"; }
	| { method: "prepareSymmetricKey", pubKey: string; }
	| { method: "registerSymmetricKey", pubKey: string, pswd: Uint8Array; }
	| { method: "signEvent", pubKey: string, event: Event; }
	| { method: "contentStarted" }
	| { method: "clientChanged" }
	| { method: "openSettings" }
	| { method: "testOptionsSender" }
	| { method: "openOptionsInTab" }
	| { method: "prepareClient", clientUrl: string }
	| { method: "clientSubmited", id: string }
	| { method: "addLogTab" }
	| { method: "getClientsDev" }

	/*
type Client = {
	uuid: string,
	displayAccount: boolean,
	publicKey: Uint8Array,
	positionX: number, positionY: number,
	backgroundColor: string, backgroundOpacity: number,
	priority: number,
	urlPattern: string
}
*/

async function
globalMethod(m: GlobalMethod, s: browser.runtime.MessageSender)
{
	console.log("message received", m);
	switch (m.method) {
		case "accountDisplayInfo":
			if (!s.url) throw new Error("bad");
			return getAccountMethod(s.url);
		case "publicKey":
			if (!s.tab) throw new Error("bad");
			if (s.tab.id === undefined) throw new Error("bad");
			if (!s.url) throw new Error("bad");
			return hex(await getPublicKey(s.url, s.tab.id));
		case "prepareSymmetricKey":
			if (!s.tab) throw new Error("bad");
			if (s.tab.id === undefined) throw new Error("bad");
			return qPswd(m.pubKey, s.tab.id);
		case "registerSymmetricKey":
			if (!s.tab) throw new Error("bad");
			if (s.tab.id === undefined) throw new Error("bad");
			return await rgSymkey(s.tab.id, m.pubKey);
		case "signEvent": return signEvent(m.pubKey, m.event);
		case "contentStarted":
			if (!s.tab) throw new Error("background.ts: contentStarted: bad1");
			if (s.tab.id === undefined) throw new Error(
				"background.ts: contentStarted: bad2");
			return pgVanished(s.tab.id);
		case "clientChanged": return broadcast({ method: "clientChanged" });
		case "openSettings": {
			console.log("background: openSettings");
			console.log(s.url);
			if (!s.url) throw new Error("bad");
			const cl = await getClient(s.url);
			let ot;
			let use;
			if (await DB.getUseClientSet()) {
				ot = await browser.tabs.create({
					active: false,
					url: browser.runtime.getURL(
						"options.html?openType=set&id=set")
				});
				if (!s.tab) throw new Error("bad");
				if (!s.tab.id) throw new Error("bad");
				if (!ot.id) throw new Error("bad");
				use = await otbs.assign("set", s.tab.id, ot.id)
			}
			else if (cl) {
				ot = await browser.tabs.create({
					active: false,
					url: browser.runtime.getURL(
						"options.html?openType=uuid&id=" + encodeURIComponent(cl.uuid) )
				});
				if (!s.tab) throw new Error("bad");
				if (!s.tab.id) throw new Error("bad");
				if (!ot.id) throw new Error("bad");
				use = await otbs.assign(cl.uuid, s.tab.id, ot.id)
			}
			console.log("openSettings:", use);
			if (!ot) throw new Error("bad");
			if (!ot.id) throw new Error("bad");
			if (use === undefined) throw new Error("bad");
			if (use !== ot.id) {
				await browser.tabs.remove(ot.id);
			}
			await browser.tabs.update(use, { active: true });
			return;
		}
		case "openOptionsInTab":
			console.log("background: openOptionsInTab");
			const ot2 = await browser.tabs.create({
				active: false,
				url: browser.runtime.getURL(
					"options.html?openType=tab&id=tab")
			});
			if (s.tab?.id === undefined) throw new Error("bad");
			if (ot2.id === undefined) throw new Error("bad");
			const use = await otbs.assign("tab", null, ot2.id)
			if (use !== ot2.id) {
				await browser.tabs.remove(ot2.id);
			}
			await browser.tabs.update(use, { active: true });
			return;
		case "prepareClient":
			console.log("background: prepareClient");
			const c = await getClient(m.clientUrl);
			console.log("background: getClient return: ", c);
			if (!s.tab) throw new Error("bad");
			if (s.tab.id === undefined) throw new Error("bad");
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
				if (!ot.id) throw new Error("bad");
				const use = await otbs.assign(m.clientUrl, s.tab.id, ot.id);
				if (use != ot.id) await browser.tabs.remove(ot.id);
				await browser.tabs.update(use, { active: true }); }

			return;
		case "clientSubmited":
			console.log("background: clientSubmited");
			const clnt = await getClient(m.id);
			console.log("background: clientSubmited:", clnt);
			if (clnt !== null) {
				if (!s.tab) throw new Error("bad");
				if (s.tab.id === undefined) throw new Error("bad");
				const sts = await otbs.complete(m.id, s.tab.id);
				for (const st of sts)
					await browser.tabs.sendMessage(st, { method: "clientReady", clientUrl: m.id });
				const st0 = sts[0]
				if (st0 !== undefined)
					await browser.tabs.update(st0, { active: true });
				if (s.tab.id === undefined) throw new Error("bad");
				await browser.tabs.remove(s.tab.id);
				return true; }
			else return false;
		case "addLogTab":
			console.log("addLogTab");
			if (s.tab?.id === undefined) throw backgroundError(
				"addLogTab: no sender tab ID" );
			Log.addLogTab(s.tab.id);
			return;
		case "getClientsDev":
			console.log("background.ts: getClientsDev");
			const clnts = await DB.getClients();
			console.log("background.ts: getClientsDev:", clnts);
			return clientsSummary(clnts);
	}
}

function
clientsSummary(cls: Client[]): ClientSummary[]
{
	return cls.map(cl => { return {
		type: "ClientSummary",
		uuid: { type: "UUID", value: cl.uuid },
		name: cl.name,
		urlPattern: cl.urlPattern }; });
}

browser.tabs.onRemoved.addListener(pgVanished);

const waitForClientChanged = new Map();

type Account = {
	name: string;
}

async function
getAccountMethod(url: string)
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
				backgroundOpacity: c.backgroundOpacity ?? 0.5,
				openSettingsByClick: c.openSettingsByClick
			};
}

type PasswordMessage = {
	result: Uint8Array
};

async function
rgSymkey(tid: number, pbk: string)
{
	const acc2 = await getAccount(unhex(pbk));
	const tid2 = (await itbs.keyInputTabs()).get(pbk);
	if (tid !== tid2) throw new Error("bad");
	const port = browser.tabs.connect(tid, { name: "readPassword_ecd3f236f8" });
	const { promise: pr, resolve: rs } = Promise.withResolvers<Uint8Array>();
	port.onMessage.addListener(m => {
		if (!("result" in m) || !(m.result instanceof Uint8Array))
			throw new Error("bad");
		rs(m.result); });
	const pswd3: Uint8Array = await pr;
	try {
		if (await acc2.checkPassword(pswd3)) {
			const { pswds = {} } =
				await browser.storage.session.get("pswds");
			pswds[pbk] = await acc2.getSymmetricKey(pswd3);
			await browser.storage.session.set({ pswds });

			console.log("*** rgSymkey ***");
			const sts = await itbs.complete(pbk, tid);
			for (const st of sts)
				await browser.tabs.sendMessage(st,
					{ method: "pswdReady", pubKey: pbk });
			const st = sts[0];
			if (st !== undefined)
				await browser.tabs.update(st, { active: true });
			await browser.tabs.remove(tid);
			console.log("*** rgSymkey: return true ***");
			return true; }
		else {	return false; } }
	finally { pswd3.fill(0); }
}

async function
signEvent(pbk: string, evt: Event)
{
			const acc = await getAccount(unhex(pbk));
			const { pswds = {} } = await browser.storage.session.get("pswds");
			const smk = pswds[pbk];
			return acc.signEvent(evt, smk);
}

const requestsWAitingForClient = new Map();

async function
getPublicKey(url: string, tid: number)
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
	if (!ot.id) throw new Error("bad");
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
getClient(url: string): Promise<Client|null>
{
	console.log("getClient begin", url);
	const cs: Client[] = await DB.getClients();
	cs.sort((a, b) => (b.priority ?? 100) - (a.priority ?? 100));
	for (const c of cs) {
		const pattern = new URLPattern(c.urlPattern);
		if (pattern.test(url)) return c; }
	return null;
}

async function
qPswd(pk: string, st: number)
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
	if (it.id === undefined) throw new Error("bad");
	const use = await itbs.assign(pk, st, it.id);
	if (use != it.id) {
//		console.log("TAB REMOVE 2", tid);
		if (!it.id) throw new Error("bad");
		await browser.tabs.remove(it.id);
	}
	await browser.tabs.update(use, { active: true });
}

function
hex(bs: Iterable<number>): string
{
	return [...bs]
		.map(b => b.toString(16).padStart(2, "0")).join("");
}

function
unhex(s: string)
{
	const bs = new Uint8Array(s.length / 2);
	for (let i = 0; i < bs.length; ++i)
		bs[i] = parseInt(s.slice(i * 2, i * 2 + 2), 16);
	return bs;
}

async function
getAccount(pbk: Uint8Array)
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
broadcast(m: object)
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
hexToRgb(hex: string)
{
	console.log("hexToRgb", hex);
	return {
		red: parseInt(hex.slice(1, 3), 16),
		green: parseInt(hex.slice(3, 5), 16),
		blue: parseInt(hex.slice(5, 7), 16)
	};
}

async function
pgVanished(vt: number)
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
	const rc0 = r.cancelled[0]?.sources[0]
	if (rc0 !== undefined) browser.tabs.update(rc0, { active: true });

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

	const sc0 = s.cancelled[0]?.sources[0];
	if (sc0 !== undefined) browser.tabs.update(sc0, { active: true });

	const ots = [...(await otbs.keyInputTabs())]
		.filter(([key]) => key !== "browser")
		.map(([, value]) => value);
	Log.setLogTabs(ots);
}

const ports = new Map();

function
ensurePort(tabId: number)
{
	let port = ports.get(tabId);
	if (port) return port;
	port = browser.tabs.connect(tabId);
	setPort(tabId, port);
	return port;
}

browser.runtime.onConnect.addListener(port => {
	const tabId = port.sender?.tab?.id;
	if (tabId === undefined) return;
	setPort(tabId, port);
});

function
setPort(tabId: number, port: browser.runtime.Port)
{
	ports.set(tabId, port);
	port.onDisconnect.addListener(() => {
		if (ports.get(tabId) === port) ports.delete(tabId);
	});
}

function
backgroundError(msg: string)
{
	new Error(`background.ts: ${msg}`);
}

async function
getOptionsTabTag(tid: number): Promise<OptionsTabTag | null>
{
	const id = await otbs.key(tid);
	if (id === null) return null;
	else return optionsTabTag(id);
}
