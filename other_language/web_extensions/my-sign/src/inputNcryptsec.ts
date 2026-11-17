const form = document.querySelector<HTMLElement>("#password-form");
const input = document.querySelector<HTMLInputElement>("#input");

if (form === null) throw new Error("bad");
form.addEventListener("submit", () => {
	console.log("inputNcryptsec.ts: SUBMIT");
});

browser.runtime.onConnect.addListener(p => {
	console.log("inputNcryptsec.ts: p:", p);
});
