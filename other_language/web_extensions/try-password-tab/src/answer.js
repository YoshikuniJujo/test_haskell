import { Mutex } from "./mutex.js"

const mutex = new Mutex;

export async function
create(a, st, it)
{
	await mutex.acquire();
	try {	let use;
		const { answers: aws = {} } =
			await browser.storage.session.get("answers");
		const aw = aws[a];
		if (aw) { aw.sourceTabs.push(st); use = aw.inputTab; }
		else {	aws[a] = {
				sourceTabs: [st], inputTab: it,
				state: "pending" }
			use = it; }
		await browser.storage.session.set({ answers: aws });
		return use; }
	finally { mutex.release(); }
}

export async function
returned(a, it)
{
	await mutex.acquire();
	try {	const { answers: aws = {} } =
			await browser.storage.session.get("answers");
		const aw = aws[a];
		if (!aw) { throw new Error(`Unknown answer: ${a}`); }
		chkInputTab(a, aw.inputTab, it);
		delete aws[a];
		await browser.storage.session.set( { answers: aws });
		return aw.sourceTabs; }
	finally { mutex.release(); }

}

function
chkInputTab(a, it0, it)
{
	if (it !== it0) {
		console.error(
			"possible attack: returnPass was received from " +
			"a tab different from the input tab",
			{	answer: a,
				expectedInputTabId: it0,
				actualSenderTabId: it } );
		throw new Error(
			"security violation: input was not returned " +
			"by the input tab associated with this answer" ); }
}

export async function
removeTab(t)
{
	await mutex.acquire();
	try {	const { answers: aws = {} } =
			await browser.storage.session.get("answers");
		const tbs = []; const rmaws = [];
		for (const[a, aw] of Object.entries(aws)) {
			const i = aw.sourceTabs.indexOf(t);
			if (i != -1) {
				aw.sourceTabs.splice(i, 1);
				if (aw.sourceTabs.length === 0) {
					tbs.push(aw.inputTab);
					delete aws[a]; } }
			else if (aw.inputTab === t) {
				rmaws.push({ answer: a, sources: aw.sourceTabs });
				delete aws[a]; } }
		await browser.storage.session.set({ answers: aws });
		return { toClose: tbs, answers: rmaws }; }
	finally { mutex.release(); }
}
