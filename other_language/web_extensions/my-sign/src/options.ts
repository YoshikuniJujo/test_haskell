import type { OptionsObject, ClientSummary, PublicKeyOption, UUID }
	from "./optionsObject.js";
import type { Client, EditingClient } from "./types.js";
import { defaultEditingClient } from "./types.js";
import { EncryptedSecretKey } from "./crypto/ncryptsec.js";
import * as DB from "./db.js"
import * as Bech32 from "./codec/bech32.js";
import { Log } from "./log2.js"
import { defaultOptionsObject } from "./optionsObject.js";
import { toHex, fromHex } from "./tools.js";
import { getElement } from "./domTools.js";

let optionsObject: OptionsObject = defaultOptionsObject();

let openType; let id;

if (location.search === "") { openType = "browser"; id = "browser"; }
else {
	const params = new URLSearchParams(location.search);
	openType = params.get("openType"); id = params.get("id"); }

if (id === null) throw new Error("bad1");

if (openType !== "url" && openType !== "uuid" && openType !== "set")
	browser.runtime.sendMessage({ method: "optionsStarted", id: id });

browser.runtime.sendMessage({ method: "addLogTab" });
browser.runtime.sendMessage({ method: "testOptionsSender" });

const clientFormD = getElement<HTMLFormElement>("#client-form-d");
const urlPatternD = getElement<HTMLInputElement>("#url-pattern-d");
const detailError = getElement<HTMLElement>("#detail-error");
const clientNameD = getElement<HTMLInputElement>("#client-name-d");
const newClient = getElement<HTMLElement>("#new-client");
const currentKeyD = getElement<HTMLSelectElement>("#current-key-d");
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
const useClientSet = getElement<HTMLInputElement>("#use-client-set");
const clientsElm = getElement<HTMLElement>("#clients");
const clientDetail = getElement<HTMLElement>("#client-detail");
const optionsError = getElement("#options-error");
const deleteClient = getElement<HTMLElement>("#delete-client");
const form = getElement("#generate-form");
const accName = getElement<HTMLInputElement>("#account-name");
const password = getElement<HTMLInputElement>("#password");
const confirm = getElement<HTMLInputElement>("#password-confirm");
const passwordError = getElement<HTMLElement>("#password-error");
const showPassword = getElement<HTMLInputElement>("#show-password");
const cancelEditClient = getElement("#cancel-edit-client");
const logOutput = getElement("#log-output");
const browserFooter = getElement<HTMLElement>("#browser-footer");
const openInTab = getElement("#open-in-tab");
const backup = getElement("#backup");
const allClear = getElement("#all-clear");
const restore = getElement("#restore");

(async () => { useClientSet.checked = await DB.getUseClientSet(); })()

useClientSet.addEventListener("change", async () =>
	{ await DB.putUseClientSet(useClientSet.checked); })

newClient.addEventListener("click", () => openNewClient());

backgroundColor.addEventListener("input", () => {
	backgroundColor16.value = backgroundColor.value;
});

async function
openNewClient(url?: string) {
	clientDetail.dataset.clientUuid = crypto.randomUUID();
	const pks = await publicKeys();
	const ec = defaultEditingClient();
	ec.urlPattern = url ?? "";
	loadClientToForm(ec, pks);

	deleteClient.hidden = true;
	clientsElm.hidden = true;
	newClient.hidden = true
	clientDetail.hidden = false;
	optionsError.textContent = "";
}

if (openType === "url") openNewClient(id);

loadPublicKeys();
loadClients();

confirm.addEventListener("input", () => {
	confirm.setCustomValidity(
		password.value === confirm.value
			? ""
			: "Passwords do not match." );
	if (password.value !== confirm.value) passwordError.hidden = false;
	else passwordError.hidden = true;
});

form.addEventListener("submit", async event => {
	event.preventDefault();

	if (password.value !== confirm.value) {
		console.log(confirm.validity.valid);
		confirm.setCustomValidity("Password do not match.");
		console.log(confirm.validity.valid);
		confirm.reportValidity();
		passwordError.textContent = "Passwords do not match.";
		return;
	}

	confirm.setCustomValidity("");

	const esk = await EncryptedSecretKey.generate(
		accName.value,
		encodePassword(password.value) );
	password.value = "";
	confirm.value = "";

	await loadPublicKeys();
});

async function
loadPublicKeys()
{
	const keys = await publicKeys();
	loadPublicKeyFrom(keys);
}

async function
publicKeys(): Promise<PublicKeyOption[]>
{
	const keys = await DB.getPublicKeysWithNames();
	return Array.from(keys, pk => {
		const hex = toHex(pk.publicKey);
		return {
			type: "PublicKeyOption",
			publicKey: hex,
			name: pk.name } });
}


showPassword.addEventListener("change", () => {
	const type = showPassword.checked ? "text" : "password";
	password.type = type;
	confirm.type = type;
});

usePriority.addEventListener("change", () => {
	priorityLabel.hidden = !usePriority.checked;
});

displayAccount.addEventListener("change", () => {
	accDisplaySettings.hidden = !displayAccount.checked;
});

async function
loadClients()
{
	const clients = await DB.getClients();
	const clientSummaries: ClientSummary[] = Array.from(clients, cl => {
		return {
			type: "ClientSummary",
			uuid: { type: "UUID", value: cl.uuid },
			name: cl.name, urlPattern: cl.urlPattern }; });
	loadClientsFromSummaries(clientSummaries);
}

function
loadClientsFromSummaries(clientSummaries: ClientSummary[])
{

	clientsElm.replaceChildren();

	for (const client of clientSummaries) {
		const row = document.createElement("div");
		row.id = client.uuid.value;
		row.dataset.name = client.name;
		row.dataset.urlPattern = client.urlPattern;
		row.textContent = client.name + " " + client.urlPattern;

		row.addEventListener("click", async () => {
			const clnt = await DB.getClient(client.uuid.value);
			clientsElm.hidden = true;
			newClient.hidden = true;
			clientDetail.hidden = false;
			clientDetail.dataset.clientUuid = client.uuid.value;
			deleteClient.hidden = false;
			optionsError.textContent = "";

			const pks = await publicKeys();
			loadClientToForm(clnt, pks);
		});
		clientsElm.append(row);
	}
}

function
loadClientToForm(client: EditingClient, pks: PublicKeyOption[])
{
	clientDetailToForm(fromEditingClient(client, pks));
}

function
clientDetailToForm(cd: ClientDetail): void
{
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

type ClientDetail = {
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

function
optionsObjectToClientDetail(obj: OptionsObject): ClientDetail
{
	return {
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
fromEditingClient(ec: EditingClient, pkos: PublicKeyOption[]): ClientDetail
{
	return {
		name: ec.name,
		urlPattern: ec.urlPattern,
		publicKey: ec.publicKey ? toHex(ec.publicKey) : "",
		publicKeyOptions: pkos,
		usePriority: ec.priority !== null,
		priority: ec.priority,
		priorityLabelHidden: ec.priority === null,
		displayAccount: ec.displayAccount,
		accountDisplaySettingsHidden: !ec.displayAccount,
		positionX: ec.positionX,
		positionY: ec.positionY,
		backgroundColor: ec.backgroundColor,
		backgroundColor16: ec.backgroundColor,
		backgroundOpacity: ec.backgroundOpacity,
		openSettingsByClick: ec.openSettingsByClick
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

clientFormD.addEventListener("submit", async event => {
	event.preventDefault();

	try {
		new URLPattern(urlPatternD.value);
		detailError.textContent = "";
	}
	catch (e) {
		if (!(e instanceof Error)) throw e;
		detailError.textContent = e.message;
		return;
	}

	const pr = usePriority.checked ? priority.valueAsNumber : null;
	if (pr === null) throw new Error("bad56");

	if (!clientDetail.dataset.clientUuid) throw new Error("badC");
	await DB.putClient({
		uuid: clientDetail.dataset.clientUuid,
		name: clientNameD.value,
		displayAccount: displayAccount.checked,
		publicKey: fromHex(currentKeyD.value),
		positionX: positionX.valueAsNumber,
		positionY: positionY.valueAsNumber,
		backgroundColor: backgroundColor16.value,
		backgroundOpacity: backgroundOpacity.valueAsNumber,
		priority: pr,
		urlPattern: urlPatternD.value,
		openSettingsByClick: openSettingsByClick.checked
	});
	await loadClients();

	await browser.runtime.sendMessage({
		method: "clientChanged"
	});

	clientDetail.hidden = true;
	clientsElm.hidden = false;
	newClient.hidden = false;

	if (openType === "url") {
		const r = await browser.runtime.sendMessage(
			{ method: "clientSubmited", id: id } );
		if (!r) optionsError.textContent = "The client does not match the original URL.";
	}
});

cancelEditClient.addEventListener("click", () => {
	clientDetail.hidden = true;
	clientsElm.hidden = false;
	newClient.hidden = false;
});

deleteClient.addEventListener("click", async () => {
	if (!clientDetail.dataset.clientUuid) throw new Error("bad");
	await DB.deleteClient(clientDetail.dataset.clientUuid);
	await loadClients();
	await browser.runtime.sendMessage({
		method: "clientChanged"
	});
	clientDetail.hidden = true;
	clientsElm.hidden = false;
	newClient.hidden = false;
});

(async () => {
	const logs: Log[] = await Log.readAll();
	console.log(logs);
	logOutput.textContent =
		logs.map(log => `${new Date(log.time).toLocaleString()} ${log.message}`)
			.join("\n");
	logOutput.scrollTop = logOutput.scrollHeight;
})()

if (openType === "browser") browserFooter.hidden = false;

openInTab.addEventListener("click", () => {
	browser.runtime.sendMessage({ method: "openOptionsInTab" });
});

type Log = { time: Date, message: string }

browser.runtime.onMessage.addListener((m, s) => {
	console.log("options.js", m, s);
	switch(m.method) {
		case "logUpdated":
			(async () => {
				console.log("LOG OUTPUT BEGIN");
				const logs: Log[] = await Log.readAll();
				console.log(logs);
				logOutput.textContent =
					logs.map(log => `${new Date(log.time).toLocaleString()} ${log.message}`)
						.join("\n");
				logOutput.scrollTop = logOutput.scrollHeight;
				return; })();
		default: return;
	}
});

function
encodePassword(pswd: string): Uint8Array
{
	return new TextEncoder().encode(pswd.normalize("NFKC"));
}

backup.addEventListener("click", async () => {
	const clnts2: ClientSummary[] = Array.from(clientsElm.children, child => {
		if (!(child instanceof HTMLElement)) throw new Error("bad");
		const nm = child.dataset.name ?? "";
		const up = child.dataset.urlPattern ?? "";
		return {
			type: "ClientSummary",
			uuid: { type: "UUID", value: child.id },
			name: nm, urlPattern: up }; });

	const crrKyOpts: PublicKeyOption[] = Array.from(currentKeyD.children, child => {
		if (!(child instanceof HTMLOptionElement)) throw new Error("bad");
		if (!child.dataset.name) throw new Error("bad");
		console.log("options.ts: BACKUP: public key value =", child.value);
		console.log("options.ts: BACKUP: public key value =", child.dataset.name);
		return {
			type: "PublicKeyOption",
			publicKey: child.value,
			name: child.dataset.name } });

	optionsObject.accountName = accName.value;
	optionsObject.showPassword = showPassword.checked;
	optionsObject.passwordErrorHidden = passwordError.hidden === true;

	optionsObject.clientsHidden = clientsElm.hidden === true;
	optionsObject.newClientButtonHidden = newClient.hidden === true;
	optionsObject.deleteClientButtonHidden = deleteClient.hidden === true;
	optionsObject.clientDetailHidden = clientDetail.hidden === true;

	optionsObject.clients = clnts2;

	optionsObject.clientName = clientNameD.value;
	optionsObject.urlPattern = urlPatternD.value;
	optionsObject.usePriority = usePriority.checked;
	optionsObject.priority = priority.valueAsNumber;
	optionsObject.displayAccount = displayAccount.checked;
	optionsObject.positionX = positionX.valueAsNumber;
	optionsObject.positionY = positionY.valueAsNumber;
	optionsObject.backgroundColor = backgroundColor.value;
	optionsObject.backgroundOpacity = backgroundOpacity.valueAsNumber;
	optionsObject.openSettingsByClick = openSettingsByClick.checked;
	optionsObject.currentKey = currentKeyD.value;
	optionsObject.currentKeyOptions = crrKyOpts;
	optionsObject.detailError = detailError.textContent;

	optionsObject.optionsError = optionsError.textContent;
	optionsObject.useClientSet = useClientSet.checked;

	console.log("BACKUP:", optionsObject);
});

allClear.addEventListener("click", () => {
	const clr = defaultOptionsObject();
	loadOptions(clr);
});

restore.addEventListener("click", () => {
	loadOptions(optionsObject);
});

function
loadOptions(obj: OptionsObject)
{
	accName.value = obj.accountName;
	showPassword.checked = obj.showPassword;
	passwordError.hidden = obj.passwordErrorHidden;

	clientsElm.hidden = obj.clientsHidden;
	newClient.hidden = obj.newClientButtonHidden;
	deleteClient.hidden = obj.deleteClientButtonHidden;
	clientDetail.hidden = obj.clientDetailHidden

	loadClientsFromSummaries(obj.clients);

	const cd = optionsObjectToClientDetail(obj);
	clientDetailToForm(cd);

	optionsError.textContent = obj.optionsError;
	useClientSet.checked = obj.useClientSet;
}
