import type { Storage } from "./storage.js";
import type { OptionsObject } from "./optionsObject.js";
import type { UUID, ClientSummary, OptionsTabTag, PublicKeyOption } from "./types.js";
import { defaultOptionsObject, newClientMode } from "./optionsObject.js";

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
		this.#optionsObject = defaultOptionsObject();
	}

	optionsObject(): OptionsObject
	{
		return  this.#optionsObject;
	}

	setUseClientSet(u: boolean)
	{
		this.#optionsObject.useClientSet = u;
	}

	setClientSummaries(css: ClientSummary[])
	{
		this.#optionsObject.clients = css;
	}

	setClientUuid(u: UUID)
	{
		this.#optionsObject.clientUuid = u;
	}

	setCurrentKeyOptions(ckos: PublicKeyOption[])
	{
		this.#optionsObject.currentKeyOptions = ckos;
	}

	setUrlPattern(u: string)
	{
		this.#optionsObject.urlPattern = u;
	}

	newClientMode()
	{
		newClientMode(this.#optionsObject);
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
