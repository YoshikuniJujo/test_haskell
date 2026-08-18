browser.runtime.onMessage.addListener((msg, sender) => {
	switch (msg.method) {
		case "queryInput": return open(msg.request, sender.tab.id)
		case "sendInput": return send(msg.request, msg.value); } });

async function
open(rid, stid)
{
	const tb = await browser.tabs.create({
		url: browser.runtime.getURL(
			`input.html?request=${encodeURIComponent(rid)}` ) });
	const { requests: rqs = {} } =
		await browser.storage.session.get("requests");
	rqs[rid] = { sourceTab: stid, inputTabId: tb.id, state: "pending" };
	await browser.storage.session.set({ requests: rqs });
}

async function
send(rid, v)
{
	const { requests: rqs = {} } =
		await browser.storage.session.get("requests");
	const rq = rqs[rid];
	if (!rq) { throw new Error(`Unknown request: ${rid}`); }
	await browser.tabs.sendMessage(
		rq.sourceTab, { method: "sendInput", request: rid, value: v } );
	await browser.tabs.update(rq.sourceTab, { active: true });
	await browser.tabs.remove(rq.inputTabId);
	delete rqs[rid]; await browser.storage.session.set({ requests: rqs });
}
