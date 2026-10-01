import { EncryptedSecretKey } from "./crypto/ncryptsec.js";
import * as DB from "./db.js"
import * as Bech32 from "./codec/bech32.js";
import { Log } from "./log2.js"
import { EnsurablePort } from "./ensurablePort.js";

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

if (id === null) throw new Error("bad");

console.log("options begin");
console.log("options.js: ", location.search);

const eport = new EnsurablePort(id);

if (openType !== "url" && openType !== "uuid" && openType !== "set") browser.runtime.sendMessage(
	{ method: "optionsStarted", id: id } );

browser.runtime.sendMessage({ method: "addLogTab" });

browser.runtime.sendMessage({ method: "testOptionsSender" });

const params = new URLSearchParams(location.search);

const newClient = document.querySelector<HTMLElement>("#new-client");
if (!newClient) throw new Error("bad");

const currentKeyD = document.querySelector<HTMLInputElement>("#current-key-d");
if (!currentKeyD) throw new Error("bad");

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

if (!backgroundColor) throw new Error("bad");
if (!backgroundColor16) throw new Error("bad");
if (!backgroundOpacity) throw new Error("bad");

if (!openSettingsByClick) throw new Error("bad");
if (!usePriority) throw new Error("bad");
if (!priorityLabel) throw new Error("bad");
if (!displayAccount) throw new Error("bad");
if (!accountDisplaySettings) throw new Error("bad");

const useClientSet = document.querySelector<HTMLInputElement>("#use-client-set");
if (!useClientSet) throw new Error("bad");

const clientsElm = document.querySelector<HTMLElement>("#clients");
if (!clientsElm) throw new Error("bad");

const clientDetail = document.querySelector<HTMLElement>("#client-detail");
if (!clientDetail) throw new Error("bad");

(async () => { useClientSet.checked = await DB.getUseClientSet(); })()

useClientSet.addEventListener("change", async () => { await DB.putUseClientSet(useClientSet.checked); })

newClient.addEventListener("click", () => openNewClient());

backgroundColor.addEventListener("input", () => {
	backgroundColor16.value = backgroundColor.value;
});

const optionsError = document.querySelector("#options-error");
const deleteClient = document.querySelector<HTMLElement>("#delete-client");

if (!optionsError) throw new Error("bad");
if (!deleteClient) throw new Error("bad");

let editingClient: Client;

type Client = {
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
		priority: null,

		openSettingsByClick: true,
		backgroundColor: "#008000",
		backgroundOpacity: 0.5,
		positionX: 100,
		positionY: 0,
		displayAccount: true

	};

	const client = editingClient;

	loadClientToForm(client);

	if (!deleteClient) throw new Error("bad");
	if (!clientsElm) throw new Error("bad");
	if (!newClient) throw new Error("bad");
	if (!clientDetail) throw new Error("bad");
	if (!optionsError) throw new Error("bad");

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

if (!confirm) throw new Error("bad");
if (!password) throw new Error("bad");
if (!passwordError) throw new Error("bad");
if (!form) throw new Error("bad");
if (!accName) throw new Error("bad");
if (!publicKeys) throw new Error("bad");
if (!showPassword) throw new Error("bad");

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

	const esk = await EncryptedSecretKey.generate(accName.value, password.value);
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

		if (!currentKeyD) throw new Error("bad");
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

	if (!clientsElm) throw new Error("bad");
	clientsElm.replaceChildren();

	if (!newClient) throw new Error("bad");
	if (!clientDetail) throw new Error("bad");
	if (!deleteClient) throw new Error("bad");
	if (!optionsError) throw new Error("bad");

	for (const client of clients) {
		const row = document.createElement("div");
		row.textContent = client.name + " " + client.urlPattern;
		row.addEventListener("click", () => {
			editingClient = client;
			console.log(editingClient.uuid);
			clients.hidden = true;
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
loadClientToForm(client: Client)
{
	if (!clientNameD) throw new Error("bad");
	const urlPatternD = document.querySelector<HTMLInputElement>("#url-pattern-d");
	if (!urlPatternD) throw new Error("bad");
	if (!currentKeyD) throw new Error("bad");
	if (!usePriority) throw new Error("bad");
	if (!priority) throw new Error("bad");
	if (!priorityLabel) throw new Error("bad");
	if (!displayAccount) throw new Error("bad");
	if (!accountDisplaySettings) throw new Error("bad");
	if (!positionX) throw new Error("bad");
	if (!positionY) throw new Error("bad");
	if (!backgroundColor) throw new Error("bad");
	if (!backgroundColor16) throw new Error("bad");
	if (!backgroundOpacity) throw new Error("bad");
	if (!openSettingsByClick) throw new Error("bad");
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
const clientNameD = document.querySelector<HTMLInputElement>("#client-name-d");
const urlPatternD = document.querySelector<HTMLInputElement>("#url-pattern-d");
const detailError = document.querySelector<HTMLElement>("#detail-error");

if (!clientFormD) throw new Error("bad");
if (!detailError) throw new Error("bad");
clientFormD.addEventListener("submit", async event => {
	console.log("clientFormD: submit");
	event.preventDefault();

	if (!clientNameD) throw new Error("bad");
	if (!urlPatternD) throw new Error("bad");
	if (!priority) throw new Error("bad");
	if (!positionX) throw new Error("bad");
	if (!positionY) throw new Error("bad");

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

	await DB.putClient(editingClient);
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
if (!cancelEditClient) throw new Error("bad");

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
	if (!logOutput) throw new Error("bad");
	const logs: Log[] = await Log.readAll();
	console.log(logs);
	logOutput.textContent =
		logs.map(log => `${new Date(log.time).toLocaleString()} ${log.message}`)
			.join("\n");
	logOutput.scrollTop = logOutput.scrollHeight;
})()

const browserFooter = document.querySelector<HTMLElement>("#browser-footer");
if (!browserFooter) throw new Error("bad");

if (openType === "browser") browserFooter.hidden = false;

const openInTab = document.querySelector("#open-in-tab");
if (!openInTab) throw new Error("bad");

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
				if (!logOutput) throw new Error("bad");
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
