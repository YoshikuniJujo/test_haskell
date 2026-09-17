const accountKey = "8450604e-6f98-49bf-a5a9-724346528a0a";
const clientKey = "6b38c0ae-8976-4966-9283-c95404411031";

export class OptionsState
{
	#storage;
	#id;
	#account;
	#client;

	constructor(id, strg = browser.storage.session)
	{
		this.#id = id;
		this.#storage = strg;
	}

	async load() {
		const data = await this.#storage.get([
			accountKey,
			clientKey ]);
		this.#account = data[accountKey]?.[this.#id] ?? {};
		this.#client = data[clientKey]?.[this.#id] ?? {};
	}

	static async create(id, strg = browser.storage.session) {
		const st = new OptionsState(id, strg);
		await st.load();
		return st;
	}

	async write(cmd, ky, vl)
	{
		switch(cmd)
		{
			case "set":
				switch(ky) {
					case "accountName":
						this.#account.name = vl;
						const data = await this.#storage.get(accountKey);
						const accs = data[accountKey] ?? {};
						accs[this.#id] = this.#account;
						await this.#storage.set({ [accountKey]: accs });
						break;
					default:
						throw new Error(`no such key: ${ky}`);
				}
				break;
			case "generateAccount":
				console.log("barbaz");
				switch(ky) {
					case "password":
						console.log("foobar");
						return {
							method: "generateAccount",
							name: this.#account.name,
							password: vl };
					default:
						console.log(cmd);
						console.log(ky);
						throw new Error(`no such key: ${ky}`);
				}
			default:
				throw new Error("no such command");
		}
	}
}
