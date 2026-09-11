import { EncryptedSecretKey } from "./crypto/ncryptsec.js";
import { encode } from "./codec/bech32.js";
import * as DB from "./db.js"
import * as Bech32 from "./codec/bech32.js";

console.log("barbaz");

const form = document.querySelector("#generate-form");
const accName = document.querySelector("#account-name");
const password = document.querySelector("#password");
const confirm = document.querySelector("#password-confirm");
const generate = document.querySelector("#generate");
const publicKeys = document.querySelector("#public-keys");

const passwordError = document.querySelector("#password-error");

const showPassword = document.querySelector("#show-password");

const currentKey = document.querySelector("#current-key");

console.log("foobar");

loadPublicKeys();

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
	currentKey.replaceChildren();
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

		currentKey.append(option);
	}
}

showPassword.addEventListener("change", () => {
	const type = showPassword.checked ? "text" : "password";
	password.type = type;
	confirm.type = type;
});

currentKey.addEventListener("change", () => {
	const npub = currentKey.value;
	console.log(npub);
});

const clientName = document.querySelector("#client-name");
const urlPattern = document.querySelector("#url-pattern");
// const publicKey = document.querySelector("#public-key");
const addClient = document.querySelector("#add-client");

const forDebug = document.querySelector("#for-debug");

addClient.addEventListener("click", async () => {
	console.log(currentKey.value);
	const client = {
		uuid: crypto.randomUUID(),
		name: clientName.value,
		urlPattern: urlPattern.value,
		publicKey: new Uint8Array(Bech32.decode(currentKey.value).dp),
		priority: 100
	};

	await DB.putClient(client);
	console.log("addClient end: ", ...await DB.getClients());

	const clients = await DB.getClients();
	forDebug.textContent = clients.map(client =>
		JSON.stringify({
			uuid: client.uuid,
			name: client.name,
			urlPattern: client.urlPattern,
			publicKey: Bech32.encode("npub", client.publicKey).slice(0, 37) + "...",
			priority: client.priority
		}, null, 2)
	).join("\n\n");
});

const useHash = document.querySelector("#use-hash");
const hash = document.querySelector("#hash");

useHash.addEventListener("change", () => {
	hash.disabled = !useHash.checked;
});
