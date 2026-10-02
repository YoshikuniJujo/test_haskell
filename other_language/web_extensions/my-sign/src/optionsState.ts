import type { Storage } from "./storage.js"

const accountKey = "8450604e-6f98-49bf-a5a9-724346528a0a";
const clientKey = "6b38c0ae-8976-4966-9283-c95404411031";

export class OptionsState
{
	#id;
	#accountStorage;
	#clientStorage;

	constructor(
		id: string,
		strg1: Storage<object> = browser.storage.session,
		strg2 = browser.storage.session )
	{
		this.#id = id;
		this.#accountStorage = strg1;
		this.#clientStorage = strg2;
	}

	async load() {
	}

	/*
	static async create(id, strg = browser.storage.session) {
		const st = new OptionsState(id, strg);
		await st.load();
		return st;
	}
	*/

	async write(cmd: string, ky: string, vl: string)
	{
		switch(cmd)
		{
			case "set":
				switch(ky) {
					case "accountName":
						const data = await this.#accountStorage.get(accountKey);
						const accs = data[accountKey] ?? {};
						await this.#accountStorage.set({ [accountKey]: accs });
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
