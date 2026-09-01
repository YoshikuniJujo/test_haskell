const answer = new URLSearchParams(location.search).get("publicKey");
const input = document.querySelector("#input");
const error = document.querySelector("#error");
const show = document.querySelector("#show-password");

input.focus();

input.addEventListener("keydown", (event) => {
	if (event.key === "Enter") {
		event.preventDefault(); sndPass(answer, input.value); } });

show.addEventListener("change", () => {
	input.type = show.checked ? "text" : "password";
});

document.querySelector("#send").addEventListener("click", () => {
	sndPass(answer, input.value); });

function
sndPass(a, v)
{
	browser.runtime.sendMessage({method: "returnPswd", pubKey: a, pswd: v});
}

browser.runtime.onMessage.addListener((m) => {
	switch (m.method) {
		case "wrongPswd":
			error.hidden = false; input.value = ""; break; } });

input.addEventListener("input", () => { error.hidden = true; });
