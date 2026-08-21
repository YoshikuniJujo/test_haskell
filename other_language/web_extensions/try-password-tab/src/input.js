const answer = new URLSearchParams(location.search).get("answer");
const input = document.querySelector("#input");
const error = document.querySelector("#error");
input.focus();

input.addEventListener("keydown", (event) => {
	if (event.key === "Enter") {
		event.preventDefault(); sndPass(answer, input.value); } });

document.querySelector("#send").addEventListener("click", () => {
	sndPass(answer, input.value); });

function
sndPass(a, v)
{
	browser.runtime.sendMessage({method: "returnPass", answer: a, val: v});
}

browser.runtime.onMessage.addListener((m) => {
	switch (m.method) {
		case "wrongPass":
			error.hidden = false; input.value = ""; break; } });

input.addEventListener("input", () => { error.hidden = true; });
