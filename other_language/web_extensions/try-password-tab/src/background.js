import * as Answer from "./answer.js";

browser.runtime.onMessage.addListener((m, s) => {
	switch (m.method) {
		case "queryInput": return qryInput(m.answer, s.tab.id);
		case "returnInput":
			return rtnInput(m.answer, m.value, s.tab.id);
		case "contentStarted":
			return pageVanished(s.tab.id); } });
browser.tabs.onRemoved.addListener(pageVanished);

async function
qryInput(rid, stid)
{
	const tb = await browser.tabs.create({
		url: browser.runtime.getURL(
			`input.html?answer=${encodeURIComponent(rid)}` ) });
	try {	await Answer.create(rid, stid, tb.id); }
	catch (e) {
		await browser.tabs.remove(tb.id); throw e; }
}

async function
rtnInput(rid, v, tid)
{
	if (v === "password") {
		const st = await Answer.returned(rid, tid);
		await browser.tabs.sendMessage(st,
			{ method: "pushInput", answer: rid, value: v });
		await browser.tabs.update(st, { active: true });
		await browser.tabs.remove(tid); }
	else {	await browser.tabs.sendMessage(
			tid, { method: "wrongPassword" }); }
}

async function
pageVanished(tid)
{
	const rslt = await Answer.removeTab(tid);
	for (const tb of rslt.toClose) await browser.tabs.remove(tb);
	for (const rq of rslt.answers)
		await browser.tabs.sendMessage(
			rq.source,
			{ method: "inputError", answer: rq.answer }); }
