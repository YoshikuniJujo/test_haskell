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

	async restore()
	{
		const obj = (await browser.storage.session.get(this.#key))[this.#key];
		console.log("optionsState.ts: restore():", this.#optionsObject);
		if (obj) {
			this.#optionsObject = obj;
			return true; }
		else  return false;
	}

	async save()
	{
		console.log("optonsState.ts: save():", this.#optionsObject);
		await browser.storage.session.set({
			[this.#key]: this.#optionsObject
		});
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

	async setClientName(cn: string)
	{
		this.#optionsObject.clientName = cn;
		await this.save();
	}

	async setClientUrlPattern(cup: string)
	{
		this.#optionsObject.urlPattern = cup;
		await this.save();
	}

	async usePriorityChanged(up: boolean)
	{
		this.#optionsObject.usePriority = up;
		await this.save();
	}

	async priorityInput(p: number)
	{
		this.#optionsObject.priority = p;
		await this.save();
	}

	async displayAccountChanged(da: boolean)
	{
		this.#optionsObject.displayAccount = da;
		await this.save();
	}

	async horizontalPositionInput(x: number)
	{
		this.#optionsObject.positionX = x;
		await this.save();
	}

	async verticalPositionInput(y: number)
	{
		this.#optionsObject.positionY = y;
		await this.save();
	}

	async backgroundColorInput(bc: string)
	{
		this.#optionsObject.backgroundColor = bc;
		await this.save();
	}

	async setAccountName(an: string)
	{
		this.#optionsObject.accountName = an;
		await this.save();
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
