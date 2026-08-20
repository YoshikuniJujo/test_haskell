import { Mutex } from "./mutex.js"

const mutex = new Mutex;

export async function
create(rid, stid, itid)
{
	await mutex.acquire();
	const { requests: rqs = {} } =
		await browser.storage.session.get("requests");
	rqs[rid] = { sourceTab: stid, inputTab: itid, state: "pending" };
	await browser.storage.session.set({ requests: rqs });
	mutex.release();
}

export async function
returned(rid, tid)
{
	await mutex.acquire();
	const { requests: rqs = {} } =
		await browser.storage.session.get("requests");
	const rq = rqs[rid];
	if (!rq) { throw new Error(`Unknown request: ${rid}`); }
	chkInputTab(rid, rq.inputTab, tid);
	delete rqs[rid];
	await browser.storage.session.set( { requests: rqs });
	mutex.release();
	return rq.sourceTab;
}

function
chkInputTab(rid, tid0, tid)
{
	if (tid !== tid0) {
		console.error(
			"possible attack: returnInput was recieved from " +
			"a tab different from the input tab",
			{	requestId: rid,
				expectedInputTabId: tid0,
				actualSenderTabId: tid } );
		throw new Error(
			"security violation: input was not returned " +
			"by the input tab associated with this request" ); }
}

export async function
removeTab(tid)
{
	await mutex.acquire();
	const { requests: rqs = {} } =
		await browser.storage.session.get("requests");
	const tabs = [];
	for (const[rid, rq] of Object.entries(rqs)) {
		if (rq.sourceTab !== tid && rq.inputTab !== tid) continue;
		if (rq.sourceTab === tid) tabs.push(rq.inputTab);
		delete rqs[rid]; }
	browser.storage.session.set({ requests: rqs });
	mutex.release();
	return tabs;
}
