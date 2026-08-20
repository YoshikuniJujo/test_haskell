import { Mutex } from "./mutex.js"

const mutex = new Mutex;

export async function
create(rid, stid, itid)
{
	await mutex.acquire();
	try {	const { requests: rqs = {} } =
			await browser.storage.session.get("requests");
		rqs[rid] = {
			sourceTab: stid, inputTab: itid, state: "pending" };
		await browser.storage.session.set({ requests: rqs }); }
	finally { mutex.release(); }
}

export async function
returned(rid, tid)
{
	await mutex.acquire();
	try {	const { requests: rqs = {} } =
			await browser.storage.session.get("requests");
		const rq = rqs[rid];
		if (!rq) { throw new Error(`Unknown request: ${rid}`); }
		chkInputTab(rid, rq.inputTab, tid);
		delete rqs[rid];
		await browser.storage.session.set( { requests: rqs });
		return rq.sourceTab; }
	finally { mutex.release(); }

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

export async function
removeTab(tid)
{
	await mutex.acquire();
	try {	const { requests: rqs = {} } =
			await browser.storage.session.get("requests");
		const tabs = [];
		const rmrqs = [];
		let found = false;
		let inputRemoved = false;
		for (const[rid, rq] of Object.entries(rqs)) {
			if (rq.sourceTab !== tid && rq.inputTab !== tid)
				continue;
			found = true;
			if (rq.sourceTab === tid) { tabs.push(rq.inputTab); }
			else {	inputRemoved = true;
				rmrqs.push({ request: rid, sourceTab: rq.sourceTab }); }
			delete rqs[rid]; }
		await browser.storage.session.set({ requests: rqs });
		if (inputRemoved) {
			return { kind: "inputRemoved", requests: rmrqs };
		} else if (found) {
			return { kind: "sourceRemoved", toClose: tabs };
		} else {
			return { kind: "nothing" };
		} }
	finally { mutex.release(); }
}
