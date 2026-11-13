import type { Storage } from "./storage.js";
import type { OptionsObject } from "./optionsObject.js";
import type {
	UUID, Client, EditingClient, ClientSummary, ClientSummaryRaw,
	ClientSet, OptionsTabTag, PublicKeyOption, Result, Profile, EditMode, Permission, KeyWithName,
	AccountSettingsRadio } from "./types.js";
import { uuid, fromUuid, EditClientMode, editModeToString, radioToPermissionValue } from "./types.js";
import { defaultOptionsObject, newClientMode, clientListMode } from "./optionsObject.js";
import { toHex, fromHex } from "./tools.js";


import { PermissionNew } from "./permission.js";

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
		console.log("OPTIONS STATE: CONSTRUCTOR: this.#optionsObject:", this.#optionsObject);
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

	async openClientDetail(
		ec: EditingClient, pkos: PublicKeyOption[], pmss: PermissionNew[])
	{
		console.log("optionsState.ts: OPEN CLIENT DETAIL:", pmss);

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
		this.#optionsObject.editModeHidden = true;

		this.setPermissions(pmss);

		console.log("optionsState.ts: editMode =", this.#optionsObject.editMode);
		switch(editModeToString(this.#optionsObject.editMode)) {
			case "Client": this.clientSettingsMode(); break;
			case "Profile": this.profileSettingsMode(); break;
			case "Permission": this.permissionSettingsMode(); break;
			default: throw new Error("never occur"); break;
		}

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
		if (obj.clientUuid === null) throw new Error("warui10");
		if (obj.currentKey === null) throw new Error("warui11");
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

	getClient(): ClientSummaryRaw
	{
		if (this.#optionsObject.clientUuid === null)
			throw new Error("warui12");
		return {
			type: "ClientSummaryRaw",
			uuid: fromUuid(this.#optionsObject.clientUuid),
			name: this.#optionsObject.clientName,
			urlPattern: this.#optionsObject.urlPattern,
			priority: this.#optionsObject.priority };
	}

	getProfile(): Profile
	{
		const obj = this.#optionsObject;
		if (obj.clientUuid === null) throw new Error("warui13");
		if (obj.currentKey === null) throw new Error("warui14");
		return {
			uuid: fromUuid(obj.clientUuid),
			publicKey: fromHex(obj.currentKey),
			openSettingsByClick: obj.openSettingsByClick,
			backgroundColor: obj.backgroundColor,
			backgroundOpacity: obj.backgroundOpacity,
			positionX: obj.positionX, positionY: obj.positionY,
			displayAccount: obj.displayAccount };
	}

	doesClientUuidExist(): boolean
	{
		return this.#optionsObject.clientUuid !== null;
	}

	clientUuid(): UUID
	{
		if (this.#optionsObject.clientUuid === null)
			throw new Error("warui15: " + this.#key);
		return this.#optionsObject.clientUuid;
	}

	urlPattern(): string
	{
		return this.#optionsObject.urlPattern;
	}

	async useClientSetChanged(us: boolean)
	{
		this.#optionsObject.useClientSet = us;
		await this.save();
	}

	selectSetMode(ssm: boolean)
	{
		this.#optionsObject.selectSetMode = ssm;
	}

	setClientSetList(csl: ClientSet[])
	{
		this.#optionsObject.clientSetList = csl;
		this.save();
	}

	setSelectAndEdit(sae: boolean)
	{
		if (sae) this.#optionsObject.selectSetOr =
			{ selectSetOr: "SelectAndEdit" }
		else this.#optionsObject.selectSetOr = { selectSetOr: "Select" }
	}

	getSelectOr()
	{
		return this.#optionsObject.selectSetOr;
	}

	async newSetNameInput(nsn: string)
	{
		this.#optionsObject.newSetName = nsn;
		await this.save();
	}

	getNewSetName()
	{
		return this.#optionsObject.newSetName;
	}

	async setCurrentSetUuid(csu: string)
	{
		this.#optionsObject.currentSetUuid = csu;
		await this.save();
	}

	doesClientDetailOpen(cu: UUID): boolean
	{
		return this.#optionsObject.clientUuid?.value === cu.value &&
			!this.#optionsObject.clientDetailHidden;
	}

	onlyOkButton()
	{
		this.#optionsObject.revertButtonHidden = true;
		this.#optionsObject.applyButtonHidden = true;
		this.#optionsObject.cancelButtonHidden = true;
	}

	onlyNextButton()
	{
		this.#optionsObject.permissionSettingsExitHidden = true;
	}

	setDetailErrorHidden(deh: boolean)
	{
		this.#optionsObject.detailErrorHidden = deh;
	}

	setUrlPatternNotMatchErrorHidden(upnmeh: boolean)
	{
		this.#optionsObject.urlPatternNotMatchErrorHidden = upnmeh;
	}

	clientSettingsMode()
	{
		this.#optionsObject.clientSettingsHidden = false;
		this.#optionsObject.deleteClientConfirmationHidden = true;
		this.#optionsObject.permissionSettingsHidden = true;
		this.#optionsObject.profileSettingsHidden = true;
	}

	deleteClientConfirmationMode()
	{
		this.#optionsObject.clientSettingsHidden = true;
		this.#optionsObject.deleteClientConfirmationHidden = false;
		this.#optionsObject.permissionSettingsHidden = true;
		this.#optionsObject.profileSettingsHidden = true;
	}

	permissionSettingsMode()
	{
		this.#optionsObject.clientSettingsHidden = true;
		this.#optionsObject.deleteClientConfirmationHidden = true;
		this.#optionsObject.permissionSettingsHidden = false;
		this.#optionsObject.profileSettingsHidden = true;
	}

	profileSettingsMode()
	{
		this.#optionsObject.clientSettingsHidden = true;
		this.#optionsObject.deleteClientConfirmationHidden = true;
		this.#optionsObject.permissionSettingsHidden = true;
		this.#optionsObject.profileSettingsHidden = false;
	}

	setEditMode(em: EditMode)
	{
		this.#optionsObject.editMode = em;
		console.log("optionsState.ts: SET EDIT MODE:", em);
		this.save();
	}

	setPermissions(pmss: PermissionNew[])
	{
		console.log("optionsState.ts: setPermissions: pmss =", pmss);
		this.#optionsObject.clientPermissions = {};
		pmss.forEach(p => {
			this.#optionsObject.clientPermissions[toHex(p.permission().publicKey)] = {
				accountName: p.account().name,
				getPublicKey: p.permission().getPublicKey,
				sign: p.permission().sign,
				encrypt: p.permission().encrypt,
				decrypt: p.permission().decrypt }
		});
	}

	modifyPermission(pk: Uint8Array, p: "none" | "publicKey" | "sign")
	{
		const pv = this.#optionsObject.clientPermissions[toHex(pk)];
		if (pv === undefined) throw new Error("warui16");
		radioToPermissionValue(pv, p);
		this.save();
	}

	getPermission(pk: Uint8Array): Permission
	{
		const pv = this.#optionsObject.clientPermissions[toHex(pk)];
		const uuid = this.#optionsObject.clientUuid;
		if (uuid === null) throw new Error("warui17");
		if (pv === undefined) throw new Error("warui18");
		return {
			clientUuid: fromUuid(uuid),
			publicKey: pk,
			getPublicKey: pv.getPublicKey,
			sign: pv.sign, encrypt: pv.encrypt, decrypt: pv.decrypt };
	}

	atLeastOnePermission(): boolean
	{
		return Object.values(this.#optionsObject.clientPermissions)
			.some(p => p.getPublicKey);
	}

	setAtLeastOnePermissionHidden(aoph: boolean): void
	{
		this.#optionsObject.atLeastOnePermissionHidden = aoph;
	}

	onlyClientOk(): void
	{
		this.#optionsObject.clientDeleteHidden = true;
		this.#optionsObject.clientRevertHidden = true;
		this.#optionsObject.clientApplyHidden = true;
		this.#optionsObject.clientCancelHidden = true;
	}

	accountListMode(): void
	{
		this.#optionsObject.accountListHidden = false;
		this.#optionsObject.accountRenameHidden = true;
		this.#optionsObject.generateFormHidden = true;
	}

	generateAccountMode(): void
	{
		this.#optionsObject.accountListHidden = true;
		this.#optionsObject.generateFormHidden = false;
		this.save();
	}

	setAccountList(ks: KeyWithName[]): void
	{
		this.#optionsObject.accountList = ks;
	}

	setAccountSettingsRadio(ar: AccountSettingsRadio): void
	{
		this.#optionsObject.accountSettingsRadio = ar;
	}

	accountSettingsRadio(): AccountSettingsRadio
	{
		return this.#optionsObject.accountSettingsRadio;
	}

	accountRenameMode(): void
	{
		this.#optionsObject.accountListHidden = true;
		this.#optionsObject.accountRenameHidden = false;
		this.save();
	}

	setAccountPublicKey(pk: Uint8Array): void
	{
		this.#optionsObject.accountPublicKey = pk;
		this.save();
	}

	accountPublicKey(): Uint8Array | null
	{
		return this.#optionsObject.accountPublicKey;
	}

	setAccountRenameName(nm: string): void
	{
		this.#optionsObject.accountRenameName = nm;
		this.save();
	}

	accountRenameName(): string
	{
		return this.#optionsObject.accountRenameName;
	}

	setCustomSaveNameChecked(): boolean
	{
		return this.#optionsObject.setSaveName;
	}

	useSetCustomSaveName(u: boolean): void
	{
		this.#optionsObject.setSaveName = u;
		this.#optionsObject.customSaveNameLabelHidden = !u;
		this.save();
	}

	customSaveName(): string
	{
		return this.#optionsObject.customSaveName;
	}

	setCustomSaveName(nm: string): void
	{
		this.#optionsObject.customSaveName = nm;
		this.save();
	}

	useSetCustomSaveNameRename(u: boolean): void
	{
		this.#optionsObject.setSaveNameRename = u;
		this.#optionsObject.customSaveNameLabelRenameHidden = !u;
		this.save();
	}

	getSetCustomSaveNameRename(): boolean
	{
		return this.#optionsObject.setSaveNameRename;
	}

	setCustomSaveNameRename(nm: string): void
	{
		this.#optionsObject.customSaveNameRename = nm;
		this.save();
	}

	customSaveNameRename(): string
	{
		return this.#optionsObject.customSaveNameRename;
	}

	accountExportMode(): void
	{
		this.#optionsObject.accountListHidden = true;
		this.#optionsObject.accountExportFormHidden = false;
		this.save();
	}
}

async function
foo(strg: Storage<OptionsObject>, key: string)
{
	const data = await strg.get(key);
//	const accs = data[key] ?? {};
//	await strg.set({ [key]: accs });
}
