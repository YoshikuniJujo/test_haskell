import * as Answer from "./answer.js";

browser.runtime.onMessage.addListener((m, s) => {
	switch (m.method) {
		case "queryPass": return qryPass(m.answer, s.tab.id);
		case "returnPass": return rtnPass(m.answer, m.val, s.tab.id);
		case "contentStarted": return pageVanished(s.tab.id); } });
browser.tabs.onRemoved.addListener(pageVanished);

async function
qryPass(a, st)
{
	const it = await browser.tabs.create({
		active: false,
		url: browser.runtime.getURL(
			`input.html?answer=${encodeURIComponent(a)}` ) });
	const use = await Answer.create(a, st, it.id);
	if (use !== it.id) await browser.tabs.remove(it.id);
	await browser.tabs.update(use, { active: true });
}

async function
rtnPass(a, v, it)
{
	if (v === "password") {
		const st = await Answer.returned(a, it);
		await browser.tabs.sendMessage(st,
			{ method: "pushPass", answer: a, value: v });
		await browser.tabs.update(st, { active: true });
		await browser.tabs.remove(it); }
	else {	await browser.tabs.sendMessage( it, { method: "wrongPass" }); }
}

async function
pageVanished(vt)
{
	const r = await Answer.removeTab(vt);
	for (const t of r.toClose) await browser.tabs.remove(t);
	for (const a of r.answers)
		await browser.tabs.sendMessage(
			a.source, { method: "passError", answer: a.answer });
}
