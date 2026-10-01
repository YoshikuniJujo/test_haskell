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

const currentKeyD = document.querySelector("#current-key-d");

const usePriority = document.querySelector<HTMLInputElement>("#use-priority");
const priorityLabel = document.querySelector("#priority-label");
const priority = document.querySelector<HTMLInputElement>("#priority");

const displayAccount = document.querySelector<HTMLInputElement>("#display-account");
const accountDisplaySettings =
	document.querySelector("#account-display-settings");
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
const deleteClient = document.querySelector("#delete-client");

if (!optionsError) throw new Error("bad");
if (!deleteClient) throw new Error("bad");

let editingClient: Client;

type Client = {
	uuid: string,
	name: string,
	urlPattern: string,
	publicKey: string | undefined,
	priority: number | null
	openSettingsByClick: boolean,
	backgroundColor: string,
	backgroundOpacity: string,
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

		openSettingByClick: true

	};

	const client = editingClient;

	loadClientToForm(client);

	deleteClient.hidden = true;
	clientsElm.hidden = true;
	newClient.hidden = true
	clientDetail.hidden = false;
	optionsError.textContent = "";
}

if (openType === "url") openNewClient(id);

const form = document.querySelector("#generate-form");
const accName = document.querySelector("#account-name");
const password = document.querySelector("#password");
const confirm = document.querySelector("#password-confirm");
// const generate = document.querySelector("#generate");
const publicKeys = document.querySelector("#public-keys");

const passwordError = document.querySelector("#password-error");

const showPassword = document.querySelector("#show-password");

console.log("foobar");

loadPublicKeys();
loadClients();

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

	clientsElm.replaceChildren();

	for (const client of clients) {
		const row = document.createElement("div");
		row.textContent = client.name + " " + client.urlPattern;
		row.addEventListener("click", () => {
			editingClient = client;
			console.log(editingClient.uuid);
			document.querySelector("#clients").hidden = true;
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
loadClientToForm(client)
{
	document.querySelector("#client-name-d").value = client.name ?? "";
	document.querySelector("#url-pattern-d").value = client.urlPattern;
	currentKeyD.value = client.publicKey
		? Bech32.encode("npub", client.publicKey)
		: "";

	usePriority.checked = client.priority !== null;
	priority.value = client.priority ?? 100;
	priorityLabel.hidden = client.priority === null;

	displayAccount.checked = client.displayAccount !== false;
	accountDisplaySettings.hidden = !displayAccount.checked;

	positionX.value = client.positionX ?? 100;
	positionY.value = client.positionY ?? 0;

	backgroundColor.value = client.backgroundColor ?? "#008000";
	backgroundColor16.value = client.backgroundColor ?? "#008000";
	backgroundOpacity.value = client.backgroundOpacity ?? 0.5;

	openSettingsByClick.checked = client.openSettingsByClick ?? true;
}

const clientFormD = document.querySelector("#client-form-d");
const clientNameD = document.querySelector("#client-name-d");
const urlPatternD = document.querySelector("#url-pattern-d");

clientFormD.addEventListener("submit", async event => {
	console.log("clientFormD: submit");
	event.preventDefault();

	editingClient.name = clientNameD.value;
	try {
		editingClient.urlPattern = urlPatternD.value;
		new URLPattern(urlPatternD.value);
		document.querySelector("#detail-error").textContent = "";
	}
	catch (e) {
		document.querySelector("#detail-error").textContent = e.message;
		return;
	}
	editingClient.publicKey =
		new Uint8Array(Bech32.decode(currentKeyD.value).dp);
	editingClient.priority = usePriority.checked ? Number(priority.value) : null;

	editingClient.displayAccount = displayAccount.checked;
	editingClient.positionX = Number(positionX.value);
	editingClient.positionY = Number(positionY.value);

	editingClient.backgroundColor = backgroundColor16.value;
	editingClient.backgroundOpacity = backgroundOpacity.value;

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
