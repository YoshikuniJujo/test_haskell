import * as Bech32 from "./codec/bech32.js"

const accName = new URLSearchParams(location.search).get("accountName");
const pubKey = new URLSearchParams(location.search).get("publicKey");
const input = document.querySelector("#input");
const show = document.querySelector("#show-password");
// const send = document.querySelector("#send");
const error = document.querySelector("#error");
const onMessage = browser.runtime.onMessage;

input.focus();

input.addEventListener("input", () => { error.hidden = true; });
show.addEventListener("change", () => {
	input.type = show.checked ? "text" : "password"; });
onMessage.addListener((m) => { switch (m.method) {
	case "wrongPswd": error.hidden = false; input.value = ""; break; } });

const form = document.querySelector("#password-form");

document.querySelector("#account-info").textContent =
	accName + " " + Bech32.encode("npub", unhex(pubKey)).slice(0, 25) + "...";

form.addEventListener("submit", (event) => {
	event.preventDefault();
	sendPswd(input.value);
});

function
sendPswd(p)
{
	browser.runtime.sendMessage({ method: "returnPswd", pubKey, pswd: p });
}

function
unhex(s)
{
	const bs = new Uint8Array(s.length / 2);
	for (let i = 0; i < bs.length; ++i)
		bs[i] = parseInt(s.slice(i * 2, i * 2 + 2), 16);
	return bs;
}
