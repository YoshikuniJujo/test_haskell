import type { Storage } from "./storage.js";
import type { OptionsObject } from "./optionsObject.js";
import type { ClientSummary, OptionsTabTag } from "./types.js";
import { defaultOptionsObject } from "./optionsObject.js";

const KEY_BASE = "8450604e-6f98-49bf-a5a9-724346528a0a";

export class OptionsState
{
	#storage;
	#key;
	#optionsObject;

	constructor(
		tag: OptionsTabTag,
		strg: Storage<OptionsObject> = browser.storage.session)
	{
		this.#storage = strg;
		this.#key = `${KEY_BASE}:${tag.id}`;
		const obj = defaultOptionsObject();

		this.#optionsObject = obj;

		return
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
