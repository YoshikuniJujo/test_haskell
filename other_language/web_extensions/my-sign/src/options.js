import { EncryptedSecretKey } from "./crypto/ncryptsec.js";
import { encode } from "./codec/bech32.js";
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
		const npub = encode("npub", new Uint8Array(pk.publicKey));
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
		const npub = encode("npub", new Uint8Array(pk.publicKey));
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

const useHashD = document.querySelector("#use-hash-d");
const fragmentLabel = document.querySelector("#fragment-label");
const hashD = document.querySelector("#hash-d");

useHashD.addEventListener("change", () => {
	fragmentLabel.hidden = !useHashD.checked;
	hashD.disabled = !useHashD.checked;
});

const usePriority = document.querySelector("#use-priority");
const priorityLabel = document.querySelector("#priority-label");
const priority = document.querySelector("#priority");

usePriority.addEventListener("change", () => {
	priorityLabel.hidden = !usePriority.checked;
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

			document.querySelector("#client-name-d").value = client.name ?? "";
			document.querySelector("#url-pattern-d").value = client.urlPattern;
			currentKeyD.value = client.publicKey
				? Bech32.encode("npub", client.publicKey)
				: "";
		});
		list.append(row);
	}
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

	document.querySelector("#client-name-d").value = client.name ?? "";
	document.querySelector("#url-pattern-d").value = client.urlPattern;
	currentKeyD.value = client.publicKey
		? Bech32.encode("npub", client.publicKey)
		: "";

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
	await DB.putClient(editingClient);
	await loadClients();

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
	document.querySelector("#client-detail").hidden = true;
	document.querySelector("#clients").hidden = false;
});
