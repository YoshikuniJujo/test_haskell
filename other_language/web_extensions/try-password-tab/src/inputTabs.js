import { Mutex } from "./mutex.js"

const STORAGE_KEY = "857e7986-2f57-4f41-b91f-3d4392a52fcf";

export class InputTabs {

	#storage; #mutex;

	constructor(str = browser.storage.session)
	{
		this.#storage = str; this.#mutex = new Mutex;
	}

	async assign(a, st, it)
	{
		await this.#mutex.acquire();
		try {	let use;
			const aws = await this.#getAnswers();
			const aw = aws[a];
			if (aw) {
				addUnique(aw.sourceTabs, st);
				use = aw.inputTab; }
			else {	aws[a] = { sourceTabs: [st], inputTab: it }
				use = it; }
			await this.#setAnswers(aws); return use; }
		finally { this.#mutex.release(); }
	}
	
	async complete(a, actit)
	{
		await this.#mutex.acquire();
		try {	const aws = await this.#getAnswers();
			const aw = aws[a];
			if (!aw) { throw new Error(`Unknown answer: ${a}`); }
			this.#verifyInputTab(a, aw.inputTab, actit);
			delete aws[a];
			await this.#setAnswers(aws);
			return aw.sourceTabs; }
		finally { this.#mutex.release(); }
	}

	async tabClosed(t)
	{
		await this.#mutex.acquire();
		try {	
			const aws = await this.#getAnswers();
			const cls = []; const rmaws = [];
			for (const[a, tbs] of Object.entries(aws)) {
				const i = tbs.sourceTabs.indexOf(t);
				if (i != -1) {
					tbs.sourceTabs.splice(i, 1);
					if (tbs.sourceTabs.length === 0) {
						cls.push(tbs.inputTab);
						delete aws[a]; } }
				else if (tbs.inputTab === t) {
					rmaws.push({
						answer: a,
						sources: tbs.sourceTabs });
					delete aws[a]; } }
			await this.#setAnswers(aws);
			return { toClose: cls, cancelled: rmaws }; }
		finally { this.#mutex.release(); }
	}
	
	#verifyInputTab(a, ex, act)
	{
		if (act !== ex) {
			console.error(
				"possible attack: returnPass was received " +
				"from a tab different from the input tab",
				{	answer: a,
					expectedInputTabId: ex,
					actualSenderTabId: act } );
			throw new Error(
				"security violation: " +
				"input was not returned by the input tab " +
				"associated with this answer" ); }
	}

	async #getAnswers()
	{
		const { [STORAGE_KEY]: aws = {} } =
			await this.#storage.get(STORAGE_KEY); return aws;
	}

	async #setAnswers(aws)
	{
		await this.#storage.set({ [STORAGE_KEY]: aws });
	}

}

function
addUnique(a, v)
{
	if (!a.includes(v)) a.push(v);
}
