import * as Request from "./request.js";

browser.runtime.onMessage.addListener((m, s) => {
	switch (m.method) {
		case "queryInput": return qryInput(m.request, s.tab.id);
		case "returnInput":
			return rtnInput(m.request, m.value, s.tab.id); } });

async function
qryInput(rid, stid)
{
	const tb = await browser.tabs.create({
		url: browser.runtime.getURL(
			`input.html?request=${encodeURIComponent(rid)}` ) });
	try {	await Request.create(rid, stid, tb.id); }
	catch (e) {
		await browser.tabs.remove(tb.id); throw e; }
}

async function
rtnInput(rid, v, tid)
{
	const st = await Request.returned(rid, tid);
	await browser.tabs.sendMessage(st,
		{ method: "pushInput", request: rid, value: v });
	await browser.tabs.update(st, { active: true });
	await browser.tabs.remove(tid);
}

browser.tabs.onRemoved.addListener(async (tid) => {
	const tbs = await Request.removeTab(tid);
	for (const tb of tbs) await browser.tabs.remove(tb); });
