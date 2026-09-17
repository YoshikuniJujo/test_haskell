const accountKey = "8450604e-6f98-49bf-a5a9-724346528a0a";
const clientKey = "6b38c0ae-8976-4966-9283-c95404411031";

export class OptionsState
{
	#storage;
	#account;
	#client;

	constructor(strg = browser.storage.session)
	{
		this.#storage = strg;
	}

	async load() {
		const data = await this.#storage.get([
			accountKey,
			clientKey ]);
		this.#account = data[accountKey] ?? {};
		this.#client = data[clientKey] ?? {};
	}

	static async create(strg = browser.storage.session) {
		const st = new OptionsState(strg);
		await st.load();
		return st;
	}

	async setAccountName(nm)
	{
		this.#account.name = nm;
		await this.#storage.set({ [accountKey]: this.#account });
	}
}
