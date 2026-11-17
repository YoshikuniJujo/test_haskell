import { encodePassword } from "./tools.js"

const form = document.querySelector<HTMLElement>("#password-form");
const input = document.querySelector<HTMLInputElement>("#input");

if (form === null) throw new Error("bad");
if (input === null) throw new Error("bad");

form.addEventListener("submit", () => {
	console.log("inputNcryptsec.ts: SUBMIT");
	browser.runtime.sendMessage({
		method: "createAccountFromNcryptsec"
	});
});

browser.runtime.onMessage.addListener(m => {
	console.log("inputNcryptsec.ts: ON MESSAGE:", m);
	if (m.from !== "background") return;
	switch (m.method) {
		case "password":
			return (async () => { return encodePassword(input.value) })();
	}
});

browser.runtime.onConnect.addListener(p => {
	console.log("inputNcryptsec.ts: p:", p);
});
