// ========================================================================
// OPTIONS TS
// ========================================================================

// ------------------------------------------------------------------------
// IMPORT MODULES

import type { OptionsObject } from "./optionsObject.js";
import type {
	Client, ClientSummary, EditingClient,
	UUID, OptionsInputMessages, PublicKeyOption, Result
	} from "./types.js";
import { defaultOptionsObject, newClientMode } from "./optionsObject.js";
import {
	defaultEditingClient, uuid, fromUuid, uuidNull,
	isError } from "./types.js";
import { EncryptedSecretKey } from "./crypto/ncryptsec.js";
import { Log } from "./log2.js"
import { getElement, scrollToBottom } from "./domTools.js";
import { toHex, fromHex } from "./tools.js";
import * as Bech32 from "./codec/bech32.js";

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
const deleteClient = getElement<HTMLElement>("#delete-client");
const applyEditClient = getElement<HTMLButtonElement>("#apply-client");
const cancelEditClient = getElement<HTMLButtonElement>("#cancel-edit-client");

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

// ------------------------------------------------------------------------
// INITIALIZATION

(async () => {
	const obj = await browser.runtime.sendMessage({
		class: "Options", method: "optionsBegin" });
	await loadOptions(obj);

	// LOG WRITE
	browser.runtime.sendMessage({ method: "addLogTab" });
	logOutput.textContent = await Log.toString();
	scrollToBottom(logOutput); })();

// ------------------------------------------------------------------------
// ADD EVENT LISTENER

// Client List

newClient.addEventListener("click", async () => {
	const obj = await browser.runtime.sendMessage(
		{ class: "Options", method: "newClient" });
	await loadOptions(obj);
});

// Client Detail

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

openSettingsByClick.addEventListener("change", () => {
	browser.runtime.sendMessage({
		class: "Options", method: "openSettingsByClickChanged",
		checked: openSettingsByClick.checked });
});

deleteClient.addEventListener("click", async () => {
	const obj = await browser.runtime.sendMessage({
		class: "Options", method: "deleteClient" });
	loadOptions(obj); });

applyEditClient.addEventListener("click", () => {
	if (!clientFormD.checkValidity()) {
		clientFormD.reportValidity(); return; }
	applyClient(); });

cancelEditClient.addEventListener("click", () => {
	sendMethodAndLoad("clientListMode"); });

document.addEventListener("keydown", event => {
	if (event.key === "Escape" && !clientDetail.hidden)
		cancelEditClient.click(); });

clientFormD.addEventListener("submit", async event => {
	event.preventDefault();
	const b = await applyClient();
	if (b) sendMethodAndLoad("clientListMode"); });

async function
applyClient()
{
	const rslt = await sendMethod("checkUrlPattern");
	if (isError(rslt)) {
		detailError.textContent = rslt.message; return false; }
	await sendMethodAndLoad("applyClient");
	await sendMethod("clientSubmited"); return true;
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
					loadOptions(obj);
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
	[accName, "accountNameInput"] ]

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

// Others

useClientSet.addEventListener("change", async () =>
	{ sendMethodChecked("useClientSetChanged", useClientSet.checked); })

openInTab.addEventListener("click", () => {
	browser.runtime.sendMessage({ method: "openOptionsInTab" });
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
	clientDetail.hidden = obj.clientDetailHidden

	loadClientsFromSummaries(obj.clients);

	const cd = optionsObjectToClientDetail(obj);
	if (cd) clientDetailToForm(cd);

	browserFooter.hidden = obj.openInTabButtonHidden;
	optionsError.textContent = obj.optionsError;
	useClientSet.checked = obj.useClientSet;
}

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
			loadOptions(obj);
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
		option.textContent = pk.name + " " + npub.slice(0, 21) + "...";
		currentKeyD.append(option);
	}
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
sendMethodChecked(mtd: string, ckd: boolean)
{
	return browser.runtime.sendMessage({
		class: "Options", method: mtd, checked: ckd });
}

async function
sendMethodAndLoad(mtd: string)
{
	await loadOptions(await sendMethod(mtd));
}
