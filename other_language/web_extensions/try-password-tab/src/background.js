import { Answer } from "./answer.js";

const answer = new Answer(browser.storage.session, "answers");

browser.runtime.onMessage.addListener((m, s) => {
	switch (m.method) {
		case "queryPass": return qryPass(answer, m.answer, s.tab.id);
		case "giveMePassword":
			return giveMePassword(m.answer, m.request, s.tab.id);
		case "returnPass": return rtnPass(answer, m.answer, m.val, s.tab.id, isSuccess);
		case "contentStarted": return pageVanished(answer, s.tab.id); } });
browser.tabs.onRemoved.addListener((tid) => pageVanished(answer, tid));

async function
giveMePassword(answer, request, tab)
{
	console.log("I think you want to get password:");
	console.log(answer);
	console.log(request);
	const { passwords = {} } =
		await browser.storage.session.get("passwords");
	console.log("******* HEREHEREHERE *******");
	console.log(passwords);
	const pss = passwords[answer];
	console.log(`giveMePassword: pss = ${pss}`)
	await browser.tabs.sendMessage(tab, {
		method: "pushPassword",
		request,
		value: pss
	});
}

function
isSuccess(answer, password)
{
	return password === "password" + answer;
}

async function
qryPass(asw, aid, st)
{
	const it = await browser.tabs.create({
		active: false,
		url: browser.runtime.getURL(
			`input.html?answer=${encodeURIComponent(aid)}` ) });
	const use = await asw.create(aid, st, it.id);
	if (use !== it.id) await browser.tabs.remove(it.id);
	await browser.tabs.update(use, { active: true });
}

async function
rtnPass(asw, aid, v, it, isSuccess)
{
	if (isSuccess(aid, v)) {

		const { passwords = {} } =
			await browser.storage.session.get("passwords");
		passwords[aid] = v;
		await browser.storage.session.set({ passwords });
		console.log(await browser.storage.session.get("passwords"));

		const { passwords: passwords2 = {} } =
			await browser.storage.session.get("passwords");
		console.log(passwords2);
		const pss = passwords2[aid];

		const sts = await asw.returned(aid, it);
		for (const s of sts)
			await browser.tabs.sendMessage(s,
				{ method: "pushPass", answer: aid, value: pss });
		await browser.tabs.update(sts[0], { active: true });
		await browser.tabs.remove(it); }
	else {	await browser.tabs.sendMessage( it, { method: "wrongPass" }); }
}

async function
pageVanished(asw, vt)
{
	const r = await asw.removeTab(vt);
	for (const t of r.toClose) await browser.tabs.remove(t);
	for (const a of r.answers)
		for (const s of a.sources)
			await browser.tabs.sendMessage(
				s, { method: "passError", answer: a.answer });
}
