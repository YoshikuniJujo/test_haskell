import type { Storage } from "./storage.js";
import type { OptionsObject } from "./optionsObject.js";
import type {
	UUID, Client, EditingClient, ClientSummary,
	OptionsTabTag, PublicKeyOption, Result }
	from "./types.js";
import { uuid, fromUuid } from "./types.js";
import { defaultOptionsObject, newClientMode, clientListMode } from "./optionsObject.js";
import { toHex, fromHex } from "./tools.js";

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

	clientListMode()
	{
		clientListMode(this.#optionsObject);
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

	async opacityInput(o: number)
	{
		this.#optionsObject.backgroundOpacity = o;
		await this.save();
	}

	async openSettingsByClickChanged(osbc: boolean)
	{
		this.#optionsObject.openSettingsByClick = osbc;
		await this.save();
	}

	async currentKeyInput(ck: string)
	{
		this.#optionsObject.currentKey = ck;
		await this.save();
	}

	async setOpenInTabButtonHidden(oitb: boolean)
	{
		this.#optionsObject.openInTabButtonHidden = oitb;
		await this.save();
	}

	async setAccountName(an: string)
	{
		this.#optionsObject.accountName = an;
		await this.save();
	}

	async openClientDetail(ec: EditingClient, pkos: PublicKeyOption[])
	{
		if (ec.uuid === null) throw new Error(
			"optionsState.ts: openClientDetail: no UUID");
		this.#optionsObject.clientUuid = uuid(ec.uuid);
		this.#optionsObject.clientName = ec.name;
		this.#optionsObject.urlPattern = ec.urlPattern;
		this.#optionsObject.currentKey =
			ec.publicKey ? toHex(ec.publicKey) : "";
		this.#optionsObject.currentKeyOptions = pkos;
		this.#optionsObject.usePriority = ec.priority !== null;
		this.#optionsObject.priority = ec.priority ?? 100;
		this.#optionsObject.displayAccount = ec.displayAccount;
		this.#optionsObject.positionX = ec.positionX;
		this.#optionsObject.positionY = ec.positionY;
		this.#optionsObject.backgroundColor = ec.backgroundColor;
		this.#optionsObject.backgroundOpacity = ec.backgroundOpacity;
		this.#optionsObject.openSettingsByClick = ec.openSettingsByClick;
		this.#optionsObject.clientsHidden = true;
		this.#optionsObject.newClientButtonHidden = true;
		this.#optionsObject.clientDetailHidden = false;
		this.#optionsObject.deleteClientButtonHidden = false;
		this.#optionsObject.optionsError = "";
		await this.save();
		return this.#optionsObject;
	}
	
	checkUrlPattern(): Result
	{
		try {
			new URLPattern(this.#optionsObject.urlPattern);
			return { result: "success" }; }
		catch (e) {
			if (!(e instanceof Error)) throw e;
			return { result: "error", message: e.message }; }
	}

	makeClient(): Client
	{
		const obj = this.#optionsObject;
		if (obj.clientUuid === null) throw new Error("bad");
		if (obj.currentKey === null) throw new Error("bad");
		return {
			uuid: fromUuid(obj.clientUuid),
			name: obj.clientName,
			urlPattern: obj.urlPattern,
			priority: obj.priority,
			displayAccount: obj.displayAccount,
			positionX: obj.positionX, positionY: obj.positionY,
			backgroundColor: obj.backgroundColor,
			backgroundOpacity: obj.backgroundOpacity,
			openSettingsByClick: obj.openSettingsByClick,
			publicKey: fromHex(obj.currentKey),
		};
	}

	clientUuid(): UUID
	{
		if (this.#optionsObject.clientUuid === null) throw new Error("bad");
		return this.#optionsObject.clientUuid;
	}
}

async function
foo(strg: Storage<OptionsObject>, key: string)
{
	const data = await strg.get(key);
//	const accs = data[key] ?? {};
//	await strg.set({ [key]: accs });
}
