import { EncryptedSecretKey } from "./crypto/ncryptsec.js";
import * as DB from "./db.js"
import * as Bech32 from "./codec/bech32.js";
import { Log } from "./log2.js"

let openType;
let id;

if (location.search === "") {
	openType = "browser";
	id = "browser"; }
else {
	const params = new URLSearchParams(location.search);
	openType = params.get("openType");
	id = params.get("id");
}

if (id === null) throw new Error("bad1");

console.log("options begin");
console.log("options.js: ", location.search);

if (openType !== "url" && openType !== "uuid" && openType !== "set") browser.runtime.sendMessage(
	{ method: "optionsStarted", id: id } );

browser.runtime.sendMessage({ method: "addLogTab" });

browser.runtime.sendMessage({ method: "testOptionsSender" });

const params = new URLSearchParams(location.search);

const clientNameD = document.querySelector<HTMLInputElement>("#client-name-d");

const newClient = document.querySelector<HTMLElement>("#new-client");
if (!newClient) throw new Error("bad2");

const currentKeyD = document.querySelector<HTMLInputElement>("#current-key-d");
if (!currentKeyD) throw new Error("bad3");

const usePriority = document.querySelector<HTMLInputElement>("#use-priority");
const priorityLabel = document.querySelector<HTMLElement>("#priority-label");
const priority = document.querySelector<HTMLInputElement>("#priority");

const displayAccount = document.querySelector<HTMLInputElement>("#display-account");
const accountDisplaySettings =
	document.querySelector<HTMLElement>("#account-display-settings");
const positionX = document.querySelector<HTMLInputElement>("#position-x");
const positionY = document.querySelector<HTMLInputElement>("#position-y");
const backgroundColor = document.querySelector<HTMLInputElement>("#background-color");
const backgroundColor16 = document.querySelector<HTMLInputElement>("#background-color-16");
const backgroundOpacity = document.querySelector<HTMLInputElement>("#background-opacity");
const openSettingsByClick = document.querySelector<HTMLInputElement>("#open-settings-by-click");

if (!backgroundColor) throw new Error("bad4");
if (!backgroundColor16) throw new Error("bad5");
if (!backgroundOpacity) throw new Error("bad6");

if (!openSettingsByClick) throw new Error("bad7");
if (!usePriority) throw new Error("bad8");
if (!priorityLabel) throw new Error("bad9");
if (!displayAccount) throw new Error("bad10");
if (!accountDisplaySettings) throw new Error("bad11");

const useClientSet = document.querySelector<HTMLInputElement>("#use-client-set");
if (!useClientSet) throw new Error("bad12");

const clientsElm = document.querySelector<HTMLElement>("#clients");
if (!clientsElm) throw new Error("bad13");

const clientDetail = document.querySelector<HTMLElement>("#client-detail");
if (!clientDetail) throw new Error("bad14");

(async () => { useClientSet.checked = await DB.getUseClientSet(); })()

useClientSet.addEventListener("change", async () => { await DB.putUseClientSet(useClientSet.checked); })

newClient.addEventListener("click", () => openNewClient());

backgroundColor.addEventListener("input", () => {
	backgroundColor16.value = backgroundColor.value;
});

const optionsError = document.querySelector("#options-error");
const deleteClient = document.querySelector<HTMLElement>("#delete-client");

if (!optionsError) throw new Error("bad15");
if (!deleteClient) throw new Error("bad16");

let editingClient: EditingClient;

type EditingClient = {
	uuid: string,
	name: string,
	urlPattern: string,
	publicKey: Uint8Array | undefined,
	priority: number | null
	openSettingsByClick: boolean,
	backgroundColor: string,
	backgroundOpacity: number,
	positionX: number, positionY: number,
	displayAccount: boolean,
}

function
openNewClient(url?: string) {
	editingClient = {
		uuid: crypto.randomUUID(),
		name: "",
		urlPattern: url ?? "",
		publicKey: undefined,
		priority: 100,

		openSettingsByClick: true,
		backgroundColor: "#008000",
		backgroundOpacity: 0.5,
		positionX: 100,
		positionY: 0,
		displayAccount: true

	};

	const client = editingClient;

	loadClientToForm(client);

	if (!deleteClient) throw new Error("bad17");
	if (!clientsElm) throw new Error("bad18");
	if (!newClient) throw new Error("bad19");
	if (!clientDetail) throw new Error("bad20");
	if (!optionsError) throw new Error("bad21");

	deleteClient.hidden = true;
	clientsElm.hidden = true;
	newClient.hidden = true
	clientDetail.hidden = false;
	optionsError.textContent = "";
}

if (openType === "url") openNewClient(id);

const form = document.querySelector("#generate-form");
const accName = document.querySelector<HTMLInputElement>("#account-name");
const password = document.querySelector<HTMLInputElement>("#password");
const confirm = document.querySelector<HTMLInputElement>("#password-confirm");
// const generate = document.querySelector("#generate");
const publicKeys = document.querySelector("#public-keys");

const passwordError = document.querySelector("#password-error");

const showPassword = document.querySelector<HTMLInputElement>("#show-password");

console.log("foobar");

loadPublicKeys();
loadClients();

if (!confirm) throw new Error("bad22");
if (!password) throw new Error("bad23");
if (!passwordError) throw new Error("bad24");
if (!form) throw new Error("bad25");
if (!accName) throw new Error("bad26");
if (!publicKeys) throw new Error("bad27");
if (!showPassword) throw new Error("bad28");

confirm.addEventListener("input", () => {
	confirm.setCustomValidity(
		password.value === confirm.value
			? ""
			: "Passwords do not match." );
	if (password.value !== confirm.value)
		passwordError.textContent = "Passwords do not match.";
	else
		passwordError.textContent = "";
});

form.addEventListener("submit", async event => {
	console.log("submit");
	event.preventDefault();

	if (password.value !== confirm.value) {
		console.log(confirm.validity.valid);
		confirm.setCustomValidity("Password do not match.");
		console.log(confirm.validity.valid);
		confirm.reportValidity();
		passwordError.textContent = "Passwords do not match.";
		return;
	}

	console.log("here");

	confirm.setCustomValidity("");

	const esk = await EncryptedSecretKey.generate(
		accName.value,
		encodePassword(password.value) );
	password.value = "";
	confirm.value = "";

	await DB.addKeyPair(esk.toObject_563e7e39d4());
	publicKeys.replaceChildren();
	currentKeyD.replaceChildren();
	const keys = await DB.getPublicKeysWithNames();
	for (const pk of keys) {
		console.log(pk);
		const npub = Bech32.encode("npub", new Uint8Array(pk.publicKey));
		const div = document.createElement("div");
		div.textContent = npub;
		publicKeys.append(div);
	}

	await loadPublicKeys();
});

async function
loadPublicKeys()
{
	const keys = await DB.getPublicKeysWithNames();
	for (const pk of keys) {
		const option = document.createElement("option");
		const npub = Bech32.encode("npub", new Uint8Array(pk.publicKey));
		option.value = npub;
		option.textContent = pk.name + " " + npub.slice(0, 21) + "...";

		if (!currentKeyD) throw new Error("bad29");
		currentKeyD.append(option.cloneNode(true));
	}
}

showPassword.addEventListener("change", () => {
	const type = showPassword.checked ? "text" : "password";
	password.type = type;
	confirm.type = type;
});

// const forDebug = document.querySelector("#for-debug");

usePriority.addEventListener("change", () => {
	priorityLabel.hidden = !usePriority.checked;
});

displayAccount.addEventListener("change", () => {
	accountDisplaySettings.hidden = !displayAccount.checked;
});

async function
loadClients()
{
	const clients = await DB.getClients();

	if (!clientsElm) throw new Error("bad30");
	clientsElm.replaceChildren();

	if (!newClient) throw new Error("bad31");
	if (!clientDetail) throw new Error("bad32");
	if (!deleteClient) throw new Error("bad33");
	if (!optionsError) throw new Error("bad34");

	for (const client of clients) {
		const row = document.createElement("div");
		row.textContent = client.name + " " + client.urlPattern;
		row.addEventListener("click", () => {
			editingClient = client;
			console.log(editingClient.uuid);
			clientsElm.hidden = true;
			newClient.hidden = true;
			clientDetail.hidden = false;
			deleteClient.hidden = false;
			optionsError.textContent = "";

			loadClientToForm(client);
		});
		clientsElm.append(row);
	}
}

function
loadClientToForm(client: EditingClient)
{
	if (!clientNameD) throw new Error("bad35");
	const urlPatternD = document.querySelector<HTMLInputElement>("#url-pattern-d");
	if (!urlPatternD) throw new Error("bad36");
	if (!currentKeyD) throw new Error("bad37");
	if (!usePriority) throw new Error("bad38");
	if (!priority) throw new Error("bad39");
	if (!priorityLabel) throw new Error("bad40");
	if (!displayAccount) throw new Error("bad41");
	if (!accountDisplaySettings) throw new Error("bad42");
	if (!positionX) throw new Error("bad43");
	if (!positionY) throw new Error("bad44");
	if (!backgroundColor) throw new Error("bad45");
	if (!backgroundColor16) throw new Error("bad46");
	if (!backgroundOpacity) throw new Error("bad47");
	if (!openSettingsByClick) throw new Error("bad48");
	clientNameD.value = client.name ?? "";
	urlPatternD.value = client.urlPattern;
	currentKeyD.value = client.publicKey
		? Bech32.encode("npub", client.publicKey)
		: "";

	usePriority.checked = client.priority !== null;
	priority.value = String(client.priority ?? 100);
	priorityLabel.hidden = client.priority === null;

	displayAccount.checked = client.displayAccount !== false;
	accountDisplaySettings.hidden = !displayAccount.checked;

	positionX.value = String(client.positionX ?? 100);
	positionY.value = String(client.positionY ?? 0);

	backgroundColor.value = client.backgroundColor ?? "#008000";
	backgroundColor16.value = client.backgroundColor ?? "#008000";
	backgroundOpacity.value = String(client.backgroundOpacity ?? 0.5);

	openSettingsByClick.checked = client.openSettingsByClick ?? true;
}

const clientFormD = document.querySelector<HTMLInputElement>("#client-form-d");
const urlPatternD = document.querySelector<HTMLInputElement>("#url-pattern-d");
const detailError = document.querySelector<HTMLElement>("#detail-error");

if (!clientFormD) throw new Error("bad49");
if (!detailError) throw new Error("bad50");
clientFormD.addEventListener("submit", async event => {
	console.log("clientFormD: submit");
	event.preventDefault();

	if (!clientNameD) throw new Error("bad51");
	if (!urlPatternD) throw new Error("bad52");
	if (!priority) throw new Error("bad53");
	if (!positionX) throw new Error("bad54");
	if (!positionY) throw new Error("bad55");

	editingClient.name = clientNameD.value;
	try {
		editingClient.urlPattern = urlPatternD.value;
		new URLPattern(urlPatternD.value);
		detailError.textContent = "";
	}
	catch (e) {
		if (!(e instanceof Error)) throw e;
		detailError.textContent = e.message;
		return;
	}
	editingClient.publicKey =
		new Uint8Array(Bech32.decode(currentKeyD.value).dp);
	editingClient.priority = usePriority.checked ? Number(priority.value) : null;

	editingClient.displayAccount = displayAccount.checked;
	editingClient.positionX = Number(positionX.value);
	editingClient.positionY = Number(positionY.value);

	editingClient.backgroundColor = backgroundColor16.value;
	editingClient.backgroundOpacity = Number(backgroundOpacity.value);

	editingClient.openSettingsByClick = openSettingsByClick.checked;

	const pr = editingClient.priority;
	if (pr === null) throw new Error("bad56");

	await DB.putClient({
		uuid: editingClient.uuid,
		name: editingClient.name,
		displayAccount: editingClient.displayAccount,
		publicKey: editingClient.publicKey,
		positionX: editingClient.positionX,
		positionY: editingClient.positionY,
		backgroundColor: editingClient.backgroundColor,
		backgroundOpacity: editingClient.backgroundOpacity,
		priority: pr,
		urlPattern: editingClient.urlPattern,
		openSettingsByClick: editingClient.openSettingsByClick
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
		console.log("clientSubmited: return:", r);
		if (!r) optionsError.textContent = "The client does not match the original URL.";
	}
});

const cancelEditClient = document.querySelector("#cancel-edit-client");
if (!cancelEditClient) throw new Error("bad57");

cancelEditClient.addEventListener("click", () => {
	clientDetail.hidden = true;
	clientsElm.hidden = false;
	newClient.hidden = false;
});

deleteClient.addEventListener("click", async () => {
	await DB.deleteClient(editingClient.uuid);
	await loadClients();
	await browser.runtime.sendMessage({
		method: "clientChanged"
	});
	clientDetail.hidden = true;
	clientsElm.hidden = false;
	newClient.hidden = false;
});

(async () => {
	console.log("LOG OUTPUT BEGIN");
	const logOutput = document.querySelector("#log-output");
	if (!logOutput) throw new Error("bad58");
	const logs: Log[] = await Log.readAll();
	console.log(logs);
	logOutput.textContent =
		logs.map(log => `${new Date(log.time).toLocaleString()} ${log.message}`)
			.join("\n");
	logOutput.scrollTop = logOutput.scrollHeight;
})()

const browserFooter = document.querySelector<HTMLElement>("#browser-footer");
if (!browserFooter) throw new Error("bad59");

if (openType === "browser") browserFooter.hidden = false;

const openInTab = document.querySelector("#open-in-tab");
if (!openInTab) throw new Error("bad60");

openInTab.addEventListener("click", () => {
	browser.runtime.sendMessage({ method: "openOptionsInTab" });
});

type Log = { time: Date, message: string }

browser.runtime.onMessage.addListener((m, s) => {
	console.log("options.js", m, s);
	switch(m.method) {
		case "logUpdated":
			(async () => {
				console.log("logUpdated");
				console.log("LOG OUTPUT BEGIN");
				const logOutput = document.querySelector("#log-output");
				if (!logOutput) throw new Error("bad61");
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
