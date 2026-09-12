import { EncryptedSecretKey } from "./crypto/ncryptsec.js";
import * as DB from "./db.js"
import * as Bech32 from "./codec/bech32.js";

console.log("barbaz");

const form = document.querySelector("#generate-form");
const accName = document.querySelector("#account-name");
const password = document.querySelector("#password");
const confirm = document.querySelector("#password-confirm");
// const generate = document.querySelector("#generate");
const publicKeys = document.querySelector("#public-keys");

const passwordError = document.querySelector("#password-error");

const showPassword = document.querySelector("#show-password");

const currentKeyD = document.querySelector("#current-key-d");

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

const usePriority = document.querySelector("#use-priority");
const priorityLabel = document.querySelector("#priority-label");
const priority = document.querySelector("#priority");

const displayAccount = document.querySelector("#display-account");
const accountDisplaySettings =
	document.querySelector("#account-display-settings");
const positionX = document.querySelector("#position-x");
const positionY = document.querySelector("#position-y");
const backgroundColor = document.querySelector("#background-color");
const backgroundOpacity = document.querySelector("#background-opacity");

usePriority.addEventListener("change", () => {
	priorityLabel.hidden = !usePriority.checked;
});

displayAccount.addEventListener("change", () => {
	accountDisplaySettings.hidden = !displayAccount.checked;
});

let editingClient;

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
			document.querySelector("#client-detail").hidden = false;
			document.querySelector("#delete-client").hidden = false;

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
	backgroundOpacity.value = client.backgroundOpacity ?? 0.5;
}

const newClient = document.querySelector("#new-client");

newClient.addEventListener("click", () => {
	editingClient = {
		uuid: crypto.randomUUID(),
		name: "",
		urlPattern: "",
		publicKey: undefined,
		priority: null
	};

	const client = editingClient;

	loadClientToForm(client);

	document.querySelector("#delete-client").hidden = true;
	document.querySelector("#clients").hidden = true;
	document.querySelector("#client-detail").hidden = false;
});

const clientFormD = document.querySelector("#client-form-d");
const clientNameD = document.querySelector("#client-name-d");
const urlPatternD = document.querySelector("#url-pattern-d");

clientFormD.addEventListener("submit", async event => {
	event.preventDefault();

	editingClient.name = clientNameD.value;
	editingClient.urlPattern = urlPatternD.value;
	editingClient.publicKey =
		new Uint8Array(Bech32.decode(currentKeyD.value).dp);
	editingClient.priority = usePriority.checked ? Number(priority.value) : null;

	editingClient.displayAccount = displayAccount.checked;
	editingClient.positionX = Number(positionX.value);
	editingClient.positionY = Number(positionY.value);

	editingClient.backgroundColor = backgroundColor.value;
	editingClient.backgroundOpacity = backgroundOpacity.value;

	await DB.putClient(editingClient);
	await loadClients();

	await browser.runtime.sendMessage({
		method: "clientChanged"
	});

	document.querySelector("#client-detail").hidden = true;
	document.querySelector("#clients").hidden = false;
});

document.querySelector("#cancel-edit-client").addEventListener("click", () => {
	document.querySelector("#client-detail").hidden = true;
	document.querySelector("#clients").hidden = false;
});

document.querySelector("#delete-client").addEventListener("click", async () => {
	await DB.deleteClient(editingClient.uuid);
	await loadClients();
	await browser.runtime.sendMessage({
		method: "clientChanged"
	});
	document.querySelector("#client-detail").hidden = true;
	document.querySelector("#clients").hidden = false;
});
