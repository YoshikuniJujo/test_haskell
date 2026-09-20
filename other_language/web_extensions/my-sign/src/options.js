import { EncryptedSecretKey } from "./crypto/ncryptsec.js";
import * as DB from "./db.js"
import * as Bech32 from "./codec/bech32.js";
import * as Log from "./log.js"

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

if (openType !== "url" && openType !== "uuid" && openType !== "set") browser.runtime.sendMessage(
	{ method: "optionsStarted", id: id } );

browser.runtime.sendMessage({ method: "testOptionsSender" });

const params = new URLSearchParams(location.search);

const newClient = document.querySelector("#new-client");

const currentKeyD = document.querySelector("#current-key-d");

const usePriority = document.querySelector("#use-priority");
const priorityLabel = document.querySelector("#priority-label");
const priority = document.querySelector("#priority");

const displayAccount = document.querySelector("#display-account");
const accountDisplaySettings =
	document.querySelector("#account-display-settings");
const positionX = document.querySelector("#position-x");
const positionY = document.querySelector("#position-y");
const backgroundColor = document.querySelector("#background-color");
const backgroundColor16 = document.querySelector("#background-color-16");
const backgroundOpacity = document.querySelector("#background-opacity");

const openSettingsByClick = document.querySelector("#open-settings-by-click");

const useClientSet = document.querySelector("#use-client-set");

(async () => { useClientSet.checked = await DB.getUseClientSet(); })()

useClientSet.addEventListener("change", async () => { await DB.putUseClientSet(useClientSet.checked); })

newClient.addEventListener("click", () => openNewClient());

backgroundColor.addEventListener("input", () => {
	backgroundColor16.value = backgroundColor.value;
});

const optionsError = document.querySelector("#options-error");

let editingClient;

function
openNewClient(url) {
	editingClient = {
		uuid: crypto.randomUUID(),
		name: "",
		urlPattern: url ?? "",
		publicKey: undefined,
		priority: null
	};

	const client = editingClient;

	loadClientToForm(client);

	document.querySelector("#delete-client").hidden = true;
	document.querySelector("#clients").hidden = true;
	document.querySelector("#new-client").hidden = true;
	document.querySelector("#client-detail").hidden = false;
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
	const list = document.querySelector("#clients");

	list.replaceChildren();

	for (const client of clients) {
		const row = document.createElement("div");
		row.textContent = client.name + " " + client.urlPattern;
		row.addEventListener("click", () => {
			editingClient = client;
			console.log(editingClient.uuid);
			document.querySelector("#clients").hidden = true;
			document.querySelector("#new-client").hidden = true;
			document.querySelector("#client-detail").hidden = false;
			document.querySelector("#delete-client").hidden = false;
			optionsError.textContent = "";

			loadClientToForm(client);
		});
		list.append(row);
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

	document.querySelector("#client-detail").hidden = true;
	document.querySelector("#clients").hidden = false;
	document.querySelector("#new-client").hidden = false;

	if (openType === "url") {
		const r = await browser.runtime.sendMessage(
			{ method: "clientSubmited", id: id } );
		console.log("clientSubmited: return:", r);
		if (!r) optionsError.textContent = "The client does not match the original URL.";
	}
});

document.querySelector("#cancel-edit-client").addEventListener("click", () => {
	document.querySelector("#client-detail").hidden = true;
	document.querySelector("#clients").hidden = false;
	document.querySelector("#new-client").hidden = false;
});

document.querySelector("#delete-client").addEventListener("click", async () => {
	await DB.deleteClient(editingClient.uuid);
	await loadClients();
	await browser.runtime.sendMessage({
		method: "clientChanged"
	});
	document.querySelector("#client-detail").hidden = true;
	document.querySelector("#clients").hidden = false;
	document.querySelector("#new-client").hidden = false;
});

(async () => {
	console.log("LOG OUTPUT BEGIN");
	const logOutput = document.querySelector("#log-output");
	const logs = await Log.readAll();
	console.log(logs);
	logOutput.textContent =
		logs.map(log => `${new Date(log.time).toLocaleString()} ${log.message}`)
			.join("\n");
	logOutput.scrollTop = logOutput.scrollHeight;
})()

const browserFooter = document.querySelector("#browser-footer");

if (openType === "browser") browserFooter.hidden = false;

const openInTab = document.querySelector("#open-in-tab");

openInTab.addEventListener("click", () => {
	browser.runtime.sendMessage({ method: "openOptionsInTab" });
});
