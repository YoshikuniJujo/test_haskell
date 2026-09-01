import { InputTabs } from "./inputTabs.js";

const itbs = new InputTabs();

browser.runtime.onMessage.addListener((m, s) => { switch (m.method) {
	case "queryPswd": return qPswd(m.pubKey, s.tab.id);
	case "getSomething":
		return gSomething(m.pubKey, m.request, m.parameter, s.tab.id);
	case "returnPswd": return rPswd(m.pubKey, m.pswd, s.tab.id, cPswd);
	case "contentStarted": return pgVanished(s.tab.id); } });
browser.tabs.onRemoved.addListener((t) => pgVanished(t));

async function
qPswd(pk, st)
{
	const { pswds = {} } = await browser.storage.session.get("pswds");
	if (pswds[pk] !== undefined) {
		await browser.tabs.sendMessage(
			st, { method: "pswdReady", pubKey: pk } ); return; }
	const it = await browser.tabs.create({
		active: false,
		url: browser.runtime.getURL(
			`input.html?publicKey=${encodeURIComponent(pk)}` ) });
	const use = await itbs.assign(pk, st, it.id);
	if (use !== it.id) await browser.tabs.remove(it.id);
	await browser.tabs.update(use, { active: true });
}

async function
gSomething(pk, rid, prm, st)
{
	const { pswds = {} } = await browser.storage.session.get("pswds");
	const pswd = pswds[pk];
	const rt = prm ? pswd + "_" + prm : pswd
	await browser.tabs.sendMessage(
		st, { method: "something", request: rid, value: rt } );
}

async function
rPswd(pk, pswd, it, chk)
{
	if (chk(pk, pswd)) {
		const { pswds = {} } =
			await browser.storage.session.get("pswds");
		pswds[pk] = pswd;
		await browser.storage.session.set({ pswds });
		const sts = await itbs.complete(pk, it);
		for (const s of sts) await browser.tabs.sendMessage(
			s, { method: "pswdReady", pubKey: pk } );
		await browser.tabs.update(sts[0], { active: true });
		await browser.tabs.remove(it); }
	else {	await browser.tabs.sendMessage(it, { method: "wrongPswd" }); }
}

function
cPswd(pk, pswd)
{
	return pswd === "password" + pk;
}

async function
pgVanished(vt)
{
	const r = await itbs.tabClosed(vt);
	for (const c of r.toClose) await browser.tabs.remove(c);
	for (const c of r.cancelled) for (const s of c.sources)
		await browser.tabs.sendMessage(
			s, { method: "inputTabClosed", pubKey: c.pubKey });
}
