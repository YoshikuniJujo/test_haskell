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
		const sts = await Answer.returned(a, it);
		for (const s of sts)
			await browser.tabs.sendMessage(s,
				{ method: "pushPass", answer: a, value: v });
		await browser.tabs.update(sts[0], { active: true });
		await browser.tabs.remove(it); }
	else {	await browser.tabs.sendMessage( it, { method: "wrongPass" }); }
}

async function
pageVanished(vt)
{
	const r = await Answer.removeTab(vt);
	for (const t of r.toClose) await browser.tabs.remove(t);
	for (const a of r.answers)
		for (const s of a.sources)
			await browser.tabs.sendMessage(
				s, { method: "passError", answer: a.answer });
}
