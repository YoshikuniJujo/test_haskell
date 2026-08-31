import { Mutex } from "./mutex.js"

const STORAGE_KEY = "857e7986-2f57-4f41-b91f-3d4392a52fcf";

export class InputTabs {

	#storage; #mutex;

	constructor(str = browser.storage.session)
	{
		this.#storage = str; this.#mutex = new Mutex;
	}

	async assign(pk, st, it)
	{
		await this.#mutex.acquire();
		try {	let use;
			const assns = await this.#getAssignments();
			const tbs = assns[pk];
			if (tbs) {
				addUnique(tbs.sourceTabs, st);
				use = tbs.inputTab; }
			else {	assns[pk] = { sourceTabs: [st], inputTab: it }
				use = it; }
			await this.#setAssignments(assns); return use; }
		finally { this.#mutex.release(); }
	}
	
	async complete(pk, actit)
	{
		await this.#mutex.acquire();
		try {	const assns = await this.#getAssignments();
			const tbs = assns[pk];
			if (!tbs) {
				throw new Error(`Unknown public key: ${pk}`); }
			this.#verifyInputTab(pk, tbs.inputTab, actit);
			delete assns[pk];
			await this.#setAssignments(assns);
			return tbs.sourceTabs; }
		finally { this.#mutex.release(); }
	}

	async tabClosed(t)
	{
		await this.#mutex.acquire();
		try {	const assns = await this.#getAssignments();
			const cls = []; const cclls = [];
			for (const[pk, tbs] of Object.entries(assns)) {
				const i = tbs.sourceTabs.indexOf(t);
				if (i != -1) {
					tbs.sourceTabs.splice(i, 1);
					if (tbs.sourceTabs.length === 0) {
						cls.push(tbs.inputTab);
						delete assns[pk]; } }
				else if (tbs.inputTab === t) {
					cclls.push({
						publicKey: pk,
						sources: tbs.sourceTabs });
					delete assns[pk]; } }
			await this.#setAssignments(assns);
			return { toClose: cls, cancelled: cclls }; }
		finally { this.#mutex.release(); }
	}
	
	#verifyInputTab(pk, ex, act)
	{
		if (act !== ex) {
			console.error(
				"possible attack: returnPass was received " +
				"from a tab different from the input tab",
				{	publicKey: pk,
					expectedInputTabId: ex,
					actualSenderTabId: act } );
			throw new Error(
				"security violation: " +
				"input was not returned by the input tab " +
				"associated with this public key" ); }
	}

	async #getAssignments()
	{
		const { [STORAGE_KEY]: assns = {} } =
			await this.#storage.get(STORAGE_KEY); return assns;
	}

	async #setAssignments(assns)
	{
		await this.#storage.set({ [STORAGE_KEY]: assns });
	}

}

function
addUnique(a, v)
{
	if (!a.includes(v)) a.push(v);
}
