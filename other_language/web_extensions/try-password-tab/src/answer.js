import { Mutex } from "./mutex.js"

export class Answer {

	#storage;
	#storageName;
	#mutex;

	constructor(str = browser.storage.session)
	{
		this.#storage = str;
		this.#storageName = crypto.randomUUID();
		this.#mutex = new Mutex;
	}

	async create(a, st, it)
	{
		await this.#mutex.acquire();
		try {	let use;
			const { [this.#storageName]: aws = {} } =
				await this.#storage.get(this.#storageName);
			const aw = aws[a];
			if (aw) {
				if (!aw.sourceTabs.includes(st))
					aw.sourceTabs.push(st);
				use = aw.inputTab; }
			else {	aws[a] = {
					sourceTabs: [st], inputTab: it,
					state: "pending" }
				use = it; }
			await this.#storage.set({ [this.#storageName]: aws });
			return use; }
		finally { this.#mutex.release(); }
	}
	
	async returned(a, it)
	{
		await this.#mutex.acquire();
		try {	const { [this.#storageName]: aws = {} } =
				await this.#storage.get(this.#storageName);
			const aw = aws[a];
			if (!aw) { throw new Error(`Unknown answer: ${a}`); }
			this.#chkInputTab(a, aw.inputTab, it);
			delete aws[a];
			await this.#storage.set( { [this.#storageName]: aws });
			return aw.sourceTabs; }
		finally { this.#mutex.release(); }
	}
	
	#chkInputTab(a, it0, it)
	{
		if (it !== it0) {
			console.error(
				"possible attack: returnPass was received " +
				"from a tab different from the input tab",
				{	answer: a,
					expectedInputTabId: it0,
					actualSenderTabId: it } );
			throw new Error(
				"security violation: " +
				"input was not returned by the input tab " +
				"associated with this answer" ); }
	}

	async removeTab(t)
	{
		await this.#mutex.acquire();
		try {	const { [this.#storageName]: aws = {} } =
				await this.#storage.get(this.#storageName);
			const tbs = []; const rmaws = [];
			for (const[a, aw] of Object.entries(aws)) {
				const i = aw.sourceTabs.indexOf(t);
				if (i != -1) {
					aw.sourceTabs.splice(i, 1);
					if (aw.sourceTabs.length === 0) {
						tbs.push(aw.inputTab);
						delete aws[a]; } }
				else if (aw.inputTab === t) {
					rmaws.push({
						answer: a,
						sources: aw.sourceTabs });
					delete aws[a]; } }
			await this.#storage.set({ [this.#storageName]: aws });
			return { toClose: tbs, answers: rmaws }; }
		finally { this.#mutex.release(); }
	}

}
