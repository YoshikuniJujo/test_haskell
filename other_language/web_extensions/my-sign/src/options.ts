// ========================================================================
// OPTIONS TS
// ========================================================================

// ------------------------------------------------------------------------
// IMPORT MODULES

import type { OptionsObject } from "./optionsObject.js";
import type {
	Client, ClientSummary, EditingClient,
	UUID, OptionsInputMessages, PublicKeyOption, Result,
	} from "./types.js";
import {
	defaultOptionsObject, newClientMode, selectOrToString
	} from "./optionsObject.js";
import {
	defaultEditingClient, uuid, fromUuid, uuidNull,
	isClientError, isInvalidUrlPattern, isUrlPatternMismatch,
	isError,
	accountToPublicKeyOption, permissionObjectToRadio,
	permissionValueToRadio } from "./types.js";
import { EncryptedSecretKey } from "./crypto/ncryptsec.js";
import { Log } from "./log2.js"
import { getElement, scrollToBottom } from "./domTools.js";
import { toHex, fromHex } from "./tools.js";
import * as Bech32 from "./codec/bech32.js";

declare const __DEV__: boolean;

// ------------------------------------------------------------------------
// CONTENTS
//
// * GET ELEMENT
// * INITIALIZATION
// * ADD EVENT LISTENER
//	+ Client List
//	+ Client Detail
//	+ Account
//	+ Input Element
//	+ Client Set
//	+ Others
// * FOR DEVELOPMENT
// * TYPES
// * FUNCTIONS

// ------------------------------------------------------------------------
// GET ELEMENT

const clientsElm = getElement<HTMLElement>("#clients");
const newClient = getElement<HTMLElement>("#new-client");
const clientDetail = getElement<HTMLElement>("#client-detail");
const clientFormD = getElement<HTMLFormElement>("#client-form-d");
const clientFormDClientSettings = getElement<HTMLFormElement>(
	"#client-form-d-client-settings" );
const clientNameD = getElement<HTMLInputElement>("#client-name-d");
const urlPatternD = getElement<HTMLInputElement>("#url-pattern-d");
const usePriority = getElement<HTMLInputElement>("#use-priority");
const priorityLabel = getElement<HTMLElement>("#priority-label");
const priority = getElement<HTMLInputElement>("#priority");
const displayAccount = getElement<HTMLInputElement>("#display-account");
const accDisplaySettings = getElement<HTMLElement>("#account-display-settings");
const positionX = getElement<HTMLInputElement>("#position-x");
const positionY = getElement<HTMLInputElement>("#position-y");
const backgroundColor = getElement<HTMLInputElement>("#background-color");
const backgroundColor16 = getElement<HTMLInputElement>("#background-color-16");
const backgroundOpacity = getElement<HTMLInputElement>("#background-opacity");
const openSettingsByClick = getElement<HTMLInputElement>("#open-settings-by-click");
const currentKeyD = getElement<HTMLSelectElement>("#current-key-d");
const detailError = getElement<HTMLElement>("#detail-error");
const urlPatternNotMatchError = getElement<HTMLElement>("#url-pattern-not-match-error");

const clientOk = getElement<HTMLButtonElement>("#send-client");

const deleteClient = getElement<HTMLElement>("#delete-client");
const applyEditClient = getElement<HTMLButtonElement>("#apply-client");
const cancelEditClient = getElement<HTMLButtonElement>("#cancel-edit-client");
const revert = getElement<HTMLButtonElement>("#revert");

const permissionSettingsExit =
	getElement<HTMLButtonElement>("#permission-settings-exit");

const permissionSettingsNext =
	getElement<HTMLButtonElement>("#permission-settings-next");

const form = getElement("#generate-form");
const accName = getElement<HTMLInputElement>("#account-name");
const password = getElement<HTMLInputElement>("#password");
const confirm = getElement<HTMLInputElement>("#password-confirm");
const showPassword = getElement<HTMLInputElement>("#show-password");
const passwordError = getElement<HTMLElement>("#password-error");
const optionsError = getElement("#options-error");
const useClientSet = getElement<HTMLInputElement>("#use-client-set");

const logOutput = getElement("#log-output");
const browserFooter = getElement<HTMLElement>("#browser-footer");
const openInTab = getElement<HTMLElement>("#open-in-tab");

const forDebug = getElement<HTMLElement>("#for-debug");
const resetDatabaseButton = getElement<HTMLInputElement>("#reset-database");

const selectSet = getElement<HTMLInputElement>("#select-set");
const setField = getElement<HTMLElement>("#set");
const notSetField = getElement<HTMLElement>("#not-set");
const selectOr = document.querySelectorAll<HTMLInputElement>(
	'input[name="select-or"]' );
const newSetForm = getElement<HTMLElement>("#new-set-form");
const newSetName = getElement<HTMLInputElement>("#new-set-name");
const newSet = getElement<HTMLInputElement>("#new-set");
const setList = getElement<HTMLElement>("#set-list");

const clientSettings = getElement<HTMLElement>("#client-settings");
const profileSettings = getElement<HTMLElement>("#profile-settings");
const editMode = getElement<HTMLElement>("#edit-mode");

const editModes =
	document.querySelectorAll<HTMLInputElement>(
		'input[name="edit-mode"]' );

const permissionSettings = getElement<HTMLElement>("#permission-settings");
const permissionSettingsField =
	getElement<HTMLElement>("#permission-settings-field");
const atLeastOnePermission = getElement<HTMLElement>("#at-least-one-permission");

const deleteClientConfirmation =
	getElement<HTMLElement>("#delete-client-confirmation");

const clientDeleteCancel =
	getElement<HTMLButtonElement>("#client-delete-cancel");

const clientDeleteOk = getElement<HTMLButtonElement>("#client-delete-ok");

const clientSettingsRevert = getElement<HTMLButtonElement>("#client-revert");
const clientSettingsApply = getElement<HTMLButtonElement>("#client-apply");
const clientSettingsCancel = getElement<HTMLButtonElement>("#client-cancel");

const accountList = getElement<HTMLElement>("#account-list");
const accountListArea = getElement<HTMLElement>("#account-list-area");
const newAccount = getElement<HTMLButtonElement>("#new-account");
const generateForm = getElement<HTMLElement>("#generate-form");
const generateFormExit = getElement<HTMLButtonElement>("#generate-form-exit");

// ------------------------------------------------------------------------
// INITIALIZATION

(async () => {
	const obj = await browser.runtime.sendMessage({
		class: "Options", method: "optionsBegin" });
	console.log("FOOBAR", obj);
	await loadOptions(obj);

	// LOG WRITE
	browser.runtime.sendMessage({ method: "addLogTab" });
	logOutput.textContent = await Log.toString();
	scrollToBottom(logOutput); })();

if (__DEV__) forDebug.hidden = false;

// ------------------------------------------------------------------------
// ADD EVENT LISTENER

// Client List

newClient.addEventListener("click", async () => {
	const obj = await browser.runtime.sendMessage(
		{ class: "Options", method: "newClient" });
	await loadOptions(obj);
});

// Client Detail

editModes.forEach(radio => {
	radio.addEventListener('change', () => {
		if (!radio.checked) return;
		const mode = radio.value;
		sendMethodWithValueString("editModeChanged", mode);
	});
});

usePriority.addEventListener("change", async () => {
	const obj = await browser.runtime.sendMessage({
		class: "Options", method: "usePriorityChanged",
		checked: usePriority.checked });
	await loadOptions(obj); });

displayAccount.addEventListener("change", async () => {
	const obj = await browser.runtime.sendMessage({
		class: "Options", method: "displayAccountChanged",
		checked: displayAccount.checked });
	await loadOptions(obj); });

backgroundColor.addEventListener("input", async () => {
	const obj = await browser.runtime.sendMessage({
		class: "Options", method: "backgroundColorInput",
		value: backgroundColor.value });
	await loadOptions(obj); });

openSettingsByClick.addEventListener("change", async () => {
	const obj = await browser.runtime.sendMessage({
		class: "Options", method: "openSettingsByClickChanged",
		checked: openSettingsByClick.checked });
	loadOptions(obj);
});

clientOk.addEventListener("click", async () => {
	if (!clientFormDClientSettings.reportValidity()) return;
	sendMethodAndLoadWithKind("clientOk", "ok");
});

deleteClient.addEventListener("click", async () => {
	const obj = await browser.runtime.sendMessage({
		class: "Options", method: "deleteClient" });
	await loadOptions(obj); });

applyEditClient.addEventListener("click", () => {
	if (!clientFormD.checkValidity()) {
		clientFormD.reportValidity(); return; }
	applyClient(false); });

cancelEditClient.addEventListener("click", () => {
	sendMethodAndLoad("clientListMode"); });

revert.addEventListener("click", () => {
	sendMethodAndLoad("revert"); });

permissionSettingsExit.addEventListener("click", () => {
	sendMethodAndLoad("clientListMode"); });

permissionSettingsNext.addEventListener("click", () => {
	sendMethodAndLoad("permissionSettingsNext"); });

document.addEventListener("keydown", event => {
	if (event.key === "Escape" && !clientDetail.hidden)
		cancelEditClient.click(); });

document.addEventListener("input", event => {
	sendMethodAndLoad("documentInput"); });

clientFormD.addEventListener("submit", async event => {
	event.preventDefault();
	const b = await applyClient(true);
});

async function
applyClient(isOk: boolean)
{
	const obj = await sendMethodWithBoolean("applyClient", isOk);
	loadOptions(obj);
}

// Account

showPassword.addEventListener("change", () => {
	const type = showPassword.checked ? "text" : "password";
	password.type = type; confirm.type = type; });

password.addEventListener("input", checkPassword);
confirm.addEventListener("input", checkPassword);

function
checkPassword()
{
	confirm.setCustomValidity(
		password.value === confirm.value
			? "" : "Passwords do not match." );
	if (password.value !== confirm.value) passwordError.hidden = false;
	else passwordError.hidden = true;
}

form.addEventListener("submit", async event => {
	event.preventDefault();
	const port = browser.runtime.connect();
	if (password.value !== confirm.value) {
		passwordError.hidden = false;
		confirm.setCustomValidity("Password do not match.");
		confirm.reportValidity(); return; }
	const pswd = encodePassword(password.value);
	try {	password.value = ""; confirm.value = "";
		port.postMessage({ method: "generate", password: pswd });
		port.onMessage.addListener(async m => {
			console.log("options.ts: GENERATE:", m);
			if (!isGenerateResponseMessage(m)) throw new Error("bad");
			switch (m.method) {
				case "generateDone":
					console.log("options.ts: generateDone");
					const obj = await browser.runtime.sendMessage({
						class: "Options",
						method: "optionsObject" });
					console.log("options.ts: GENERATE:", obj);
					await loadOptions(obj);
					break; } }); }
	finally { pswd.fill(0); }
	sendMethodAndLoad("currentKeyOptions");
});

type GenerateResponseMessage =
	| { method: "generateDone" }

function
isGenerateResponseMessage(m: unknown): m is GenerateResponseMessage {
	return typeof m === "object"
		&& m !== null
		&& "method" in m
		&& m.method === "generateDone";
}

// Input Element

type ValueElement = {
	addEventListener: HTMLElement["addEventListener"];
	value: string;
};

const inputElementList: [ValueElement, string][] = [
	[clientNameD, "clientNameInput"],
	[urlPatternD, "clientUrlPatternInput"],
	[backgroundColor16, "backgroundColorInput"],
	[currentKeyD, "currentKeyInput"],
	[accName, "accountNameInput"],
	[newSetName, "newSetNameInput"] ]

inputElementList.forEach(([e, m]) => {
	e.addEventListener("input", () => {
		browser.runtime.sendMessage({
			class: "Options", method: m, value: e.value }); }); });

const inputElementNumberList: [HTMLInputElement, string][] = [
	[priority, "priorityInput"],
	[positionX, "horizontalPositionInput"],
	[positionY, "verticalPositionInput"],
	[backgroundOpacity, "opacityInput"]
]

inputElementNumberList.forEach(([e, m]) => {
	e.addEventListener("input", () => {
		browser.runtime.sendMessage({
			class: "Options", method: m,
			value: e.valueAsNumber }); }); });

// Client Set

selectSet.addEventListener("click", async () => {
	const obj = await sendMethod("selectSetMode");
	if (obj !== undefined) await loadOptions(obj);
});

selectOr.forEach(r =>
	r.addEventListener("change", async () => {
		if(r.checked) {
			console.log("options.ts: selectOr: check", r.value);
			const obj = await sendMethod(r.value);
			await loadOptions(obj);
		}
	})
);

newSet.addEventListener("click", async () => {
	const obj = await sendMethod("newSet");
	loadOptions(obj);
});

// Others

useClientSet.addEventListener("change", async () => {
	const obj = await sendMethodChecked(
		"useClientSetChanged", useClientSet.checked );
	if (obj !== undefined) loadOptions(obj);
})

openInTab.addEventListener("click", () => {
	browser.runtime.sendMessage({ method: "openOptionsInTab" });
});

if (__DEV__)
	resetDatabaseButton.addEventListener("click", () => {
		browser.runtime.sendMessage({ method: "resetDatabase" });
	});

clientDeleteCancel.addEventListener("click", () => {
	sendMethodAndLoad("deleteClientCancel"); });

clientDeleteOk.addEventListener("click", () => {
	sendMethodAndLoad("deleteClientConfirmed");
});

clientSettingsRevert.addEventListener("click", () => {
	sendMethodAndLoad("clientSettingsRevert");
});

clientSettingsApply.addEventListener("click", () => {
	if (!clientFormDClientSettings.reportValidity()) return;
	sendMethodAndLoadWithKind("clientOk", "apply");
});

clientSettingsCancel.addEventListener("click", () => {
	sendMethodAndLoad("clientListMode");
});

newAccount.addEventListener("click", () => {
	sendMethodAndLoad("generateFormMode");
});

generateFormExit.addEventListener("click", () => {
	sendMethodAndLoad("accountListMode");
});

// ------------------------------------------------------------------------

browser.runtime.onMessage.addListener((m, s) => {
	console.log("options.js: onMessage:", m, s);
	if (m.class !== "Options" || m.direction !== "signal") return;
	switch(m.method) {
		case "logUpdated":
			(async () => {
				console.log("LOG OUTPUT BEGIN");
				logOutput.textContent = await Log.toString();
				scrollToBottom(logOutput);
				return; })();
		case "update": (async () => {
			console.log("options.ts: recieve UPDATE signal");
			const obj = await browser.runtime.sendMessage({
				class: "Options",
				direction: "request",
				method: "loadCommonState" });
			loadOptions(obj);
			return; })();
		default: return;
	}
});

// ------------------------------------------------------------------------
// TYPES

type InputMessages = OptionsInputMessages<OptionsObject>;

type ClientDetail = {
	uuid: UUID;
	name: string;
	urlPattern: string;
	publicKey: string | null;
	publicKeyOptions: PublicKeyOption[];
	usePriority: boolean;
	priority: number | null;
	priorityLabelHidden: boolean;
	displayAccount: boolean;
	accountDisplaySettingsHidden: boolean;
	positionX: number;
	positionY: number;
	backgroundColor: string;
	backgroundColor16: string;
	backgroundOpacity: number;
	openSettingsByClick: boolean;
}

// ------------------------------------------------------------------------
// FUNCTIONS

function
loadOptions(obj: OptionsObject)
{
	accName.value = obj.accountName;

	clientsElm.hidden = obj.clientsHidden;
	newClient.hidden = obj.newClientButtonHidden;
	deleteClient.hidden = obj.deleteClientButtonHidden;
	clientDetail.hidden = obj.clientDetailHidden;

	revert.hidden = obj.revertButtonHidden;
	applyEditClient.hidden = obj.applyButtonHidden;
	cancelEditClient.hidden = obj.cancelButtonHidden;

	loadClientsFromSummaries(obj.clients);

	const cd = optionsObjectToClientDetail(obj);
	if (cd) clientDetailToForm(cd);

	browserFooter.hidden = obj.openInTabButtonHidden;
	optionsError.textContent = obj.optionsError;
	useClientSet.checked = obj.useClientSet;
	selectSet.hidden = !obj.useClientSet;

	setField.hidden = !obj.selectSetMode;
	notSetField.hidden = obj.selectSetMode;

	const selectSetOr = getElement<HTMLInputElement>(
		"#" + selectOrToString(obj.selectSetOr) );
	if (selectSetOr === null) throw new Error("bad");
	selectSetOr.checked = true;
	console.log("options.ts: SELECT OR:", selectOrToString(obj.selectSetOr));
	newSetForm.hidden = selectOrToString(obj.selectSetOr) !== "select-and-edit"

	setList.replaceChildren();
	for (const st of obj.clientSetList) {
		console.log("options.ts: loadOptions: SET:", st.uuid);
		console.log("options.ts: loadOptions: CURRENT SET:", obj.currentSetUuid);
		const row = document.createElement("button");
		row.className = "set";
		row.textContent = st.name;
		row.classList.toggle("inactive", st.uuid !== obj.currentSetUuid);
		row.addEventListener("click", async () => {
			console.log("options.ts: set clicked:", st);
			const obj = await browser.runtime.sendMessage({
				class: "Options",
				method: "selectSet",
				value: st.uuid });
			loadOptions(obj);
		});
		setList.append(row);
	}

	newSetName.value = obj.newSetName;

	detailError.hidden = obj.detailErrorHidden;
	urlPatternNotMatchError.hidden = obj.urlPatternNotMatchErrorHidden;

	clientSettings.hidden = obj.clientSettingsHidden;
	deleteClientConfirmation.hidden = obj.deleteClientConfirmationHidden;
	permissionSettingsField.hidden = obj.permissionSettingsHidden;
	profileSettings.hidden = obj.profileSettingsHidden;
	editMode.hidden = obj.editModeHidden;

	console.log("options.ts: LOAD OPTIONS: permissions =", obj.clientPermissions);
	permissionSettings.replaceChildren();
	for (const [pk, p] of Object.entries(obj.clientPermissions)) {
		console.log("options.ts: loadOptions: p =", p);
		const accnm = p.accountName;
		const row = document.createElement("div");
		console.log("options.ts: loadOptions: PUBLIC KEY OPTION:", accnm);
		row.classList.add("permission");
		row.classList.add("permission-row");
		const accountName = document.createElement("span");
		accountName.className = "account-name";
		accountName.textContent = publicKeyToString(
			fromHex(pk), accnm );
		accountName.title = accountName.textContent;
		row.append(accountName);
			/*
		row.textContent = publicKeyToString(
			p.permission.publicKey, accnm );
			*/
		console.log(
			"options.ts: loadOptions: p.permission.publicKey:", pk);
		const rs = mkRadioGroup(
			pk,
			permissionValueToRadio(p),
			[	{ value: "none", label: "None" },
				{ value: "publicKey", label: "Get Public Key" },
				{ value: "sign", label: "+ Sign" } ],
			async value => {
				const obj = await browser.runtime.sendMessage({
					class: "Options",
					method: "permissionChanged",
					publicKey: fromHex(pk),
					value });
				await loadOptions(obj);

			}
		);
		rs.classList.add("radio-group");
		row.append(rs);
		permissionSettings.append(row);
	}

	permissionSettingsExit.hidden = obj.permissionSettingsExitHidden;
	atLeastOnePermission.hidden = obj.atLeastOnePermissionHidden;

	deleteClient.hidden = obj.clientDeleteHidden;
	clientSettingsRevert.hidden = obj.clientRevertHidden;
	clientSettingsApply.hidden = obj.clientApplyHidden;
	clientSettingsCancel.hidden = obj.clientCancelHidden;

	accountList.hidden = obj.accountListHidden;
	generateForm.hidden = obj.generateFormHidden;

	accountListArea.textContent = "HERE YOU ARE!"

	console.log("options.ts: LOAD OPTIONS: accountList =", obj.accountList);
	for (const acc of obj.accountList) {
		const row = document.createElement("button");
		row.textContent = publicKeyToString(acc.publicKey, acc.name); // Bech32.encode("npub", acc.publicKey);
		row.className = "account";
		row.addEventListener("click", () => {
			console.log("CLICKED:", acc);
		});
		accountListArea.append(row);
	}
}

function
mkRadioGroup(
	name: string,
	selectedValue: string,
	options: { value: string; label: string}[],
	onChange: (value: string) => void
): HTMLDivElement
{
	const div = document.createElement("div");

	for (const o of options) {
		const label = document.createElement("label");
		const radio = document.createElement("input");
		radio.type = "radio";
		radio.name = name;
		radio.value = o.value;
		radio.checked = o.value === selectedValue;

		radio.addEventListener("change", () => {
			if (radio.checked) onChange(radio.value);
		});

		label.append(radio, o.label);
		div.append(label); }

	return div;
}

let popoverTimer: ReturnType<typeof setTimeout> | undefined;

function
loadClientsFromSummaries(clientSummaries: ClientSummary[])
{

	clientsElm.replaceChildren();

	for (const client of clientSummaries) {
		const row = document.createElement("button");
		row.className = "client";
		row.id = client.uuid.value;
		row.dataset.name = client.name;
		row.dataset.urlPattern = client.urlPattern;
		row.textContent = client.name + " " + client.urlPattern;

		row.addEventListener("click", async () => {
			const obj = await browser.runtime.sendMessage({
				class: "Options",
				method: "openClientDetail",
				value: client.uuid });
			if (obj) loadOptions(obj);
			else {
				console.log("CLIENT ALREADY OPEN");
				if (popoverTimer !== undefined)
					clearTimeout(popoverTimer);
				const msg = getElement<HTMLElement>("#client-is-already-open");
				msg.showPopover()
				popoverTimer = setTimeout(() => {
					msg.hidePopover();
				}, 5000);
			}
		});
		clientsElm.append(row);
	}
}

function
clientDetailToForm(cd: ClientDetail): void
{
	clientDetail.dataset.clientUuid = fromUuid(cd.uuid);
	clientNameD.value = cd.name;
	urlPatternD.value = cd.urlPattern;
	loadPublicKeyFrom(cd.publicKeyOptions);
	currentKeyD.value = cd.publicKey ?? "";
	usePriority.checked = cd.usePriority;
	priority.valueAsNumber = cd.priority ?? 100;
	priorityLabel.hidden = cd.priorityLabelHidden;
	displayAccount.checked = cd.displayAccount;
	accDisplaySettings.hidden = cd.accountDisplaySettingsHidden;
	positionX.valueAsNumber = cd.positionX;
	positionY.valueAsNumber = cd.positionY;
	backgroundColor.value = cd.backgroundColor;
	backgroundColor16.value = cd.backgroundColor16;
	backgroundOpacity.valueAsNumber = cd.backgroundOpacity;
	openSettingsByClick.checked = cd.openSettingsByClick;
}

function
optionsObjectToClientDetail(obj: OptionsObject): ClientDetail | null
{
	if (!obj.clientUuid) return null;
	return {
		uuid: obj.clientUuid,
		name: obj.clientName,
		urlPattern: obj.urlPattern,
		publicKey: obj.currentKey,
		publicKeyOptions: obj.currentKeyOptions,
		usePriority: obj.usePriority,
		priority: obj.priority,
		priorityLabelHidden: !obj.usePriority,
		displayAccount: obj.displayAccount,
		accountDisplaySettingsHidden: !obj.displayAccount,
		positionX: obj.positionX,
		positionY: obj.positionY,
		backgroundColor: obj.backgroundColor,
		backgroundColor16: obj.backgroundColor,
		backgroundOpacity: obj.backgroundOpacity,
		openSettingsByClick: obj.openSettingsByClick
	};
}

function
loadPublicKeyFrom(pkos: PublicKeyOption[])
{
	currentKeyD.replaceChildren();
	for (const pk of pkos) {
		const option = document.createElement("option");
		const hex = pk.publicKey;
		option.value = hex;
		option.dataset.name = pk.name;

		const npub = Bech32.encode("npub", fromHex(pk.publicKey));
		option.textContent = publicKeyToString(fromHex(pk.publicKey), pk.name);
		currentKeyD.append(option);
	}
}

function
publicKeyToString(pk: Uint8Array, nm: string): string
{
	const npub = Bech32.encode("npub", pk);
	return nm + " " + npub.slice(0, 21) + "..." + npub.slice(-6);
}

function
encodePassword(pswd: string): Uint8Array
{
	return new TextEncoder().encode(pswd.normalize("NFKC"));
}

function
sendMethod(mtd: string)
{
	return browser.runtime.sendMessage({ class: "Options", method: mtd });
}

function
sendMethodWithBoolean(mtd: string, b: boolean)
{
	return browser.runtime.sendMessage({
		class: "Options", method: mtd, boolean: b });
}

function
sendMethodWithValueString(mtd: string, v: string)
{
	return browser.runtime.sendMessage({
		class: "Options", method: mtd, value: v });
}

function
sendMethodChecked(mtd: string, ckd: boolean): Promise<OptionsObject | undefined>
{
	return browser.runtime.sendMessage({
		class: "Options", method: mtd, checked: ckd });
}

async function
sendMethodAndLoad(mtd: string)
{
	await loadOptions(await sendMethod(mtd));
}

async function
sendMethodAndLoadWithKind(mtd: string, knd: string)
{
	await loadOptions(await browser.runtime.sendMessage({
		class: "Options",
		method: mtd,
		kind: knd }));
}
