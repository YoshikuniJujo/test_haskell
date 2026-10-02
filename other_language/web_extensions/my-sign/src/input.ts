import * as Bech32 from "./codec/bech32.js"

const params = new URLSearchParams(location.search);
const accName = params.get("accountName");
const pubKey = params.get("publicKey");
const input = document.querySelector<HTMLInputElement>("#input");
const show = document.querySelector<HTMLInputElement>("#show-password");
const error = document.querySelector<HTMLElement>("#error");
const accountInfo = document.querySelector("#account-info")
const form = document.querySelector("#password-form");

if (!input) throw new Error("bad");
if (!error) throw new Error("bad");
if (!show) throw new Error("bad");
if (!accountInfo) throw new Error("bad");
if (!form) throw new Error("bad");
if (!pubKey) throw new Error("bad");

input.focus();

input.addEventListener("input", () => { error.hidden = true; });
show.addEventListener("change", () => {
	input.type = show.checked ? "text" : "password"; });

accountInfo.textContent =
	accName + " " + Bech32.encode("npub", unhex(pubKey)).slice(0, 25) + "...";

let password: Uint8Array;

form.addEventListener("submit", async (event) => {
	event.preventDefault();
	password = encodePassword(input.value);
	// DENGEROUS REMOVE IT
	console.log("input.ts:", password);
	input.value = "";
	try {	const ok = await browser.runtime.sendMessage({
			method: "registerSymmetricKey", pubKey, pswd: password });
		if (!ok) { error.hidden = false; input.value = ""; } }
	catch(e) {
		password.fill(0);
		throw e; }
});

function
unhex(s: string)
{
	const bs = new Uint8Array(s.length / 2);
	for (let i = 0; i < bs.length; ++i)
		bs[i] = parseInt(s.slice(i * 2, i * 2 + 2), 16);
	return bs;
}

function
encodePassword(pswd: string): Uint8Array
{
	return new TextEncoder().encode(pswd.normalize("NFKC"));
}

type Message = { method: "readPassword_ecd3f236f8" }

browser.runtime.onMessage.addListener((m: Message, s) => {
	console.log("input.ts:", m, s);
	switch (m.method) {
		case "readPassword_ecd3f236f8":
			// DENGEROUS REMOVE IT
			console.log("input.ts:", password);
			try {
				console.log("input.ts: readPassword");
				const { promise: pr, resolve: rs } = Promise.withResolvers();
				rs(password);
				return pr; }
			finally { /* password.fill(0); */ }
	}
});

browser.runtime.onConnect.addListener(p => {
	console.log("input.ts: port =", p);
	try {	switch (p.name) {
			case "readPassword_ecd3f236f8":
				p.postMessage({ result: password });
				break; } }
	finally{
		password.fill(0);
		p.disconnect(); }
});
