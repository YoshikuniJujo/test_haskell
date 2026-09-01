const pubKey = new URLSearchParams(location.search).get("publicKey");
const input = document.querySelector("#input");
const show = document.querySelector("#show-password");
const send = document.querySelector("#send");
const error = document.querySelector("#error");
const onMessage = browser.runtime.onMessage;

input.focus();

input.addEventListener("keydown", (event) => { if (event.key === "Enter") {
	event.preventDefault(); sendPswd(input.value); } });
send.addEventListener("click", () => { sendPswd(input.value); });
input.addEventListener("input", () => { error.hidden = true; });
show.addEventListener("change", () => {
	input.type = show.checked ? "text" : "password"; });
onMessage.addListener((m) => { switch (m.method) {
	case "wrongPswd": error.hidden = false; input.value = ""; break; } });

function
sendPswd(p)
{
	browser.runtime.sendMessage({ method: "returnPswd", pubKey, pswd: p });
}
