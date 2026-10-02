import type { Storage } from "./storage.js"
import type { OptionsObject, ClientSummary, UUID } from "./optionsObject.js"

const KEY_BASE = "8450604e-6f98-49bf-a5a9-724346528a0a";

export class OptionsState
{
	#storage;
	#key;

	constructor(id: string, strg: Storage<OptionsObject> = browser.storage.session)
	{
		this.#storage = strg;
		this.#key = `${KEY_BASE}:${id}`;
	}

	async write(cmd: string, ky: string, vl: string)
	{
		switch(cmd)
		{
		}
	}
}

async function
foo(strg: Storage<OptionsObject>, key: string)
{
	const data = await strg.get(key);
//	const accs = data[key] ?? {};
//	await strg.set({ [key]: accs });
}
