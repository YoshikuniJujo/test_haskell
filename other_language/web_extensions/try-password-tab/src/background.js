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
	const { requests: rqs = {} } =
		await browser.storage.session.get("requests");
	rqs[rid] = { sourceTab: stid, inputTab: tb.id, state: "pending" };
	await browser.storage.session.set({ requests: rqs });
}

async function
rtnInput(rid, v, tid)
{
	const { requests: rqs = {} } =
		await browser.storage.session.get("requests");
	const rq = rqs[rid];
	if (!rq) { throw new Error(`Unknown request: ${rid}`); }
	chkInputTab(rid, rq.inputTab, tid);
	await browser.tabs.sendMessage(
		rq.sourceTab, { method: "pushInput", request: rid, value: v } );
	await browser.tabs.update(rq.sourceTab, { active: true });
	await browser.tabs.remove(rq.inputTab);
	delete rqs[rid]; await browser.storage.session.set({ requests: rqs });
}

function
chkInputTab(rid, tid0, tid)
{
	if (tid !== tid0) {
		console.error(
			"possible attack: returnInput was received from " +
			"a tab different from the input tab",
			{	requestId: rid,
				expectedInputTabId: tid0,
				actualSenderTabId: tid } );
		throw new Error(
			"security violation: input was not returned " +
			"by the input tab associated with this request" ); }
}

browser.tabs.onRemoved.addListener(async (tid) => {
	const { requests: rqs = {} } =
		await browser.storage.session.get("requests");
	let changed = false;
	const tabs = [];
	for (const[rid, rq] of Object.entries(rqs)) {
		if (rq.sourceTab !== tid && rq.inputTab !== tid) continue;
		if (rq.sourceTab === tid) tabs.push(rq.inputTab);
		delete rqs[rid];
		changed = true; }
	if (changed) await browser.storage.session.set({ requests: rqs });
	for (const tid of tabs) await browser.tabs.remove(tid); });
