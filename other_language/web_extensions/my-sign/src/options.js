import { EncryptedSecretKey } from "./crypto/ncryptsec.js";
import { encode } from "./codec/bech32.js";
import * as DB from "./db.js"

console.log("barbaz");

const form = document.querySelector("#generate-form");
const password = document.querySelector("#password");
const confirm = document.querySelector("#password-confirm");
const generate = document.querySelector("#generate");
const publicKeys = document.querySelector("#public-keys");

const passwordError = document.querySelector("#password-error");

const showPassword = document.querySelector("#show-password");

const currentKey = document.querySelector("#current-key");

console.log("foobar");

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

	const esk = await EncryptedSecretKey.generate(password.value);
	password.value = "";
	confirm.value = "";
	const npub = encode("npub", esk.publicKey);

	/*
	const div = document.createElement("div");
	div.textContent = npub;
	publicKeys.append(div);
	*/

	await DB.addKeyPair(esk.toObject_563e7e39d4());
	publicKeys.replaceChildren();
	currentKey.replaceChildren();
	const keys = await DB.getPublicKeys();
	for (const pk of keys) {
		console.log(pk);
		const npub = encode("npub", new Uint8Array(pk));
		const div = document.createElement("div");
		div.textContent = npub;
		publicKeys.append(div);

		const option = document.createElement("option");
		option.value = npub;
		option.textContent = npub;

		currentKey.append(option);
	}
});

showPassword.addEventListener("change", () => {
	const type = showPassword.checked ? "text" : "password";
	password.type = type;
	confirm.type = type;
});

currentKey.addEventListener("change", () => {
	const npub = currentKey.value;
	console.log(npub);
});
